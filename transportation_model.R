library("readxl")
library(tidyverse)
library(janitor)
library(zipcode)
library(geosphere)
library(modelr)

results <- read_excel("data/game_survey.xlsx")

results <- remove_empty(results, which = c("cols"))

results <- results %>% 
  select(Method = "18. How do you typically get to the arena?",
         Home_zip = "53. What is the five-digit zip code of your primary residence?",
         Work_zip = "55. What is the five-digit zip code of your primary place of employment?",
         Transportation_satisfaction = "14. [Topic:Getting to and from the arena (traffic & parking, public transportation, etc.); Category:A; Question:How satisfied are you with each of the following experiences when attending a game?]",
         Transportation_importance = "15. [Topic:Getting to and from the arena (traffic & parking, public transportation, etc.); Category:A; Question:Please rank the following experiences in order of their importance to your overall satisfaction with the game.]",
         Transportation_impact = "75. [Topic:Getting to and from arena; Category:A; Question:How much does getting to and from the arena (e.g. traffic, parking) impact your willingness to attend a game?]"
         ) %>% 
  filter((!is.na(Method) | !is.na(Home_zip) | !is.na(Work_zip)) & (!is.na(Transportation_satisfaction) | !is.na(Transportation_importance) | is.na(Transportation_impact)))

results[results == "7 - Extremely satisfied"] <- "7"
results[results == "7 - A lot"] <- "7"
results[results == "1 - Not at all satisfied"] <- "1"
results[results == "1 - Not satisfied at all"] <- "1"
results[results == "1 - Not at all"] <- "1"

results$Home_zip <- sapply(results$Home_zip, function(x) toString(x))
results$Work_zip <- sapply(results$Work_zip, function(x) toString(x))

results[results == "NA"] <- NA

non_zero_start <- results %>% 
  filter(nchar(Home_zip) == 5)
zero_start <- results %>% 
  filter(nchar(Home_zip) == 4) %>% 
  mutate(Home_zip = paste("0", Home_zip, sep = ""))

results <- rbind(non_zero_start, zero_start)

non_zero_start <- results %>% 
  filter(nchar(Work_zip) == 5)
zero_start <- results %>% 
  filter(nchar(Work_zip) == 4) %>% 
  mutate(Work_zip = paste("0", Work_zip, sep = ""))

results <- rbind(non_zero_start, zero_start) %>% 
  arrange(desc(Home_zip))

results <- results[-1, ] %>% 
  mutate(Transportation_satisfaction = as.numeric(Transportation_satisfaction), 
         Transportation_importance = as.numeric(Transportation_importance),
         Transportation_impact = as.numeric(Transportation_impact))

wfc <- c(-75.1720, 39.9012)
## 39.9012
data(zipcode)
zipcode <- as_tibble(zipcode)

get_dist <- function(long, lat) {
  home <- c(long, lat)
  distHaversine(wfc, home)
}

results <- as_tibble(merge(results,zipcode[c('zip','latitude','longitude')], by.x = 'Home_zip', by.y = 'zip'))
results <- results %>% 
  rowwise %>% 
  mutate(Linear_dist = get_dist(longitude, latitude))

model <- lm(Transportation_satisfaction ~ Linear_dist + Method, data = results)

results <- results %>% add_predictions(model) %>% add_residuals(model)
rmse <- sqrt(mean(results$resid^2))

