library("readxl")
library(tidyverse)
library(janitor)
library(devtools)
library(choroplethrZip)

results <- read_excel("data/game_survey.xlsx") %>% select(
  "14. [Topic:Getting to and from the arena (traffic & parking, public transportation, etc.); Category:A; Question:How satisfied are you with each of the following experiences when attending a game?]",
  "53. What is the five-digit zip code of your primary residence?")


names(results) <- c("value", "region")
results[results == "7 - Extremely satisfied"] <- 7
results[results == "1 - Not at all satisfied"] <- 1
results <- results %>% 
  filter(!is.na(region))


numericize <- function(x) {
  as.numeric(x)
}

results <- results %>% 
  mutate_all(numericize)

results$region <- sapply(results$region, function(x) toString(x))
non_zero_start <- results %>% 
  filter(nchar(region) == 5)
zero_start <- results %>% 
  filter(nchar(region) < 5) %>% 
  mutate(region = paste("0", region, sep = ""))

results <- rbind(non_zero_start, zero_start)

unique <- results %>% 
  group_by(region) %>% 
  summarize(regionx = region[[1]], value = length(value))

freq <- results %>% 
  group_by(region) %>% 
  add_tally() %>% 
  select(region, n) %>% 
  distinct()



names(freq) <- c("region", "value")

zip_choropleth(unique,
               msa_zoom = "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD",
               title    = "Transportation satisfaction by home zip code",
               legend   = "Satisfaction")

zip_choropleth(freq,
               msa_zoom = "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD",
               title    = "Survey responders home locations",
               legend   = "Fans")

