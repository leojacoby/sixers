library("readxl")
library(tidyverse)
library(janitor)
library(modelr)
library(MASS)
library(plotly)
library(ggthemes)


results <- read_excel("../data/game_survey.xlsx")

results <- remove_empty(results, which = c("cols"))

locations <- results %>% 
  dplyr::select(
  Szn_location = "132. In which part of the arena is your membership?",
  Seat_location = "131. In which part of the arena do you most frequently purchase seats?"
  ) %>% 
  mutate(Seat_location = ifelse(is.na(Seat_location) & !is.na(Szn_location), Szn_location, Seat_location)) %>% 
  select(-Szn_location)

satisfaction_results <- results %>% 
  dplyr::select(
    Transportation_satisfaction = "14. [Topic:Getting to and from the arena (traffic & parking, public transportation, etc.); Category:A; Question:How satisfied are you with each of the following experiences when attending a game?]",
    Enter_satisfaction = "14. [Topic:Entering and exiting the arena (Will Call, security, ticket takers); Category:A; Question:How satisfied are you with each of the following experiences when attending a game?]",
    Tech_satisfaction = "14. [Topic:In-arena technology (smartphone usage, mobile data, mobile app); Category:A; Question:How satisfied are you with each of the following experiences when attending a game?]",
    Food_satisfaction = "14. [Topic:In-arena food & beverage; Category:A; Question:How satisfied are you with each of the following experiences when attending a game?]",
    Retail_satisfaction = "14. [Topic:In-arena retail (including team store and kiosks); Category:A; Question:How satisfied are you with each of the following experiences when attending a game?]",
    Game_satisfaction = "14. [Topic:In-game entertainment; Category:A; Question:How satisfied are you with each of the following experiences when attending a game?]",
    Seat_satisfaction = "14. [Topic:In-seat experience (seat location, seat comfort, service from ushers, hawkers); Category:A; Question:How satisfied are you with each of the following experiences when attending a game?]"
  )

importance_results <- results %>% 
  dplyr::select(
    Transportation_importance = "15. [Topic:Getting to and from the arena (traffic & parking, public transportation, etc.); Category:A; Question:Please rank the following experiences in order of their importance to your overall satisfaction with the game.]",
    Enter_importance = "15. [Topic:Entering and exiting the arena (Will Call, security, ticket takers); Category:A; Question:Please rank the following experiences in order of their importance to your overall satisfaction with the game.]",
    Tech_importance = "15. [Topic:In-arena technology (smartphone usage, mobile data, mobile app); Category:A; Question:Please rank the following experiences in order of their importance to your overall satisfaction with the game.]",
    Food_importance = "15. [Topic:In-arena food & beverage; Category:A; Question:Please rank the following experiences in order of their importance to your overall satisfaction with the game.]",
    Retail_importance = "15. [Topic:In-arena retail (including team store and kiosks); Category:A; Question:Please rank the following experiences in order of their importance to your overall satisfaction with the game.]",
    Game_importance = "15. [Topic:In-game entertainment; Category:A; Question:Please rank the following experiences in order of their importance to your overall satisfaction with the game.]",
    Seat_importance = "15. [Topic:In-seat experience (seat location, seat comfort, service from ushers, hawkers); Category:A; Question:Please rank the following experiences in order of their importance to your overall satisfaction with the game.]"
  )

satisfaction_results[satisfaction_results == "7 - Extremely satisfied"] <- "7"
satisfaction_results[satisfaction_results == "1 - Not at all satisfied"] <- "1"
satisfaction_results[satisfaction_results == "1 - Not satisfied at all"] <- "1"
satisfaction_results[satisfaction_results == "NA"] <- NA


## standardizing on one column not the whole table

satisfaction_results <- satisfaction_results %>% 
  mutate_all(as.numeric)

importance_results <- importance_results %>% 
  mutate_all(as.numeric)

tblMean <- function(tbl) {
  cols <- colnames(tbl)
  all_values <- numeric()
  for(colname in cols) {
    col <- tbl[[colname]]
    all_values <- c(all_values, col)
  }
  return (mean(all_values, na.rm = TRUE))
}

tblSd <- function(tbl) {
  cols <- colnames(tbl)
  all_values <- numeric()
  for(colname in cols) {
    col <- tbl[[colname]]
    all_values <- c(all_values, col)
  }
  return (sd(all_values, na.rm = TRUE))
}

zStandardize <- function(x) {
  mu <- tblMean(satisfaction_results)
  sigma <- tblSd(satisfaction_results)
  return( (x - mu) / sigma )
}

rStandardize <- function(x) {
  adjusted_x <- 2*(4-x) + x
  return ( adjusted_x / 4)
}

satisfaction_results <- satisfaction_results %>% 
  mutate_all(zStandardize)

importance_results <- importance_results %>% 
  mutate_all(rStandardize)

results <- cbind(locations, satisfaction_results, importance_results) %>% 
  mutate(wTransportation = Transportation_satisfaction * Transportation_importance,
         wEnter = Enter_satisfaction * Enter_importance,
         wTech = Tech_satisfaction * Transportation_importance,
         wFood = Food_satisfaction * Food_importance,
         wRetail = Retail_satisfaction * Retail_importance,
         wGame = Game_satisfaction *Game_importance,
         wSeat = Seat_satisfaction * Seat_importance)

summary <- results %>% 
  summarize(Transportation_score = mean(wTransportation, na.rm = TRUE),
            Enter_score = mean(wEnter, na.rm = TRUE),
            Tech_score = mean(wTech, na.rm = TRUE),
            Food_score = mean(wFood, na.rm = TRUE),
            Retail_score = mean(wRetail, na.rm = TRUE),
            Game_score = mean(wGame, na.rm = TRUE),
            Seat_score = mean(wSeat, na.rm = TRUE))

weighted_satisfaction_hist <- function(col) {
  friendly_name = str_sub(col, 2)
  ggplot(results) +
    geom_histogram(aes_string(x = col, y = "..density.."), bins = 10) +
    stat_function(fun=dnorm,
                  color="red",
                  args=list(mean=mean(results[[col]], na.rm = TRUE), 
                            sd=sd(results[[col]], na.rm = TRUE))) +
    geom_vline(xintercept = mean(results[[col]], na.rm = TRUE), color = "blue") +
    geom_vline(xintercept = mean(results[[col]], na.rm = TRUE) + sd(results[[col]], na.rm = TRUE), color = "green") +
    geom_vline(xintercept = mean(results[[col]], na.rm = TRUE) - sd(results[[col]], na.rm = TRUE), color = "green") +
    labs(title = paste("Weighted", friendly_name, "Satisfaction Distribution"), x = paste("weighted", friendly_name, "satisfaction")) +
    theme_solarized() +
    NULL
}

weighted_satisfaction_hist("wTransportation")
weighted_satisfaction_hist("wFood")
weighted_satisfaction_hist("wGame")
weighted_satisfaction_hist("wSeat")
weighted_satisfaction_hist("wEnter")
weighted_satisfaction_hist("wTech")
weighted_satisfaction_hist("wRetail")

importance_summary <- results %>% 
  summarize(Transportation = mean(Transportation_importance, na.rm = TRUE),
            Game = mean(Game_importance, na.rm = TRUE),
            Food = mean(Food_importance, na.rm = TRUE),
            Retail = mean(Retail_importance, na.rm = TRUE),
            Enter = mean(Enter_importance, na.rm = TRUE),
            Tech = mean(Tech_importance, na.rm = TRUE),
            Seat = mean(Seat_importance, na.rm = TRUE))

importance_summary <- cbind(tribble(~category, "importance"), importance_summary)

flipped_importance_summary <- data.frame(t(importance_summary[-1]))
colnames(flipped_importance_summary) <- importance_summary[, 1]
flipped_importance_summary$category <- rownames(flipped_importance_summary)

ggplot(flipped_importance_summary) + 
  geom_bar(aes(x = category, y = importance), stat = "identity") +
  theme_solarized() +
  labs(y = "relative importance", x = "component", title = "Relative importance of game experience components") +
  NULL

lower_results <- results %>% filter(Seat_location == "Lower level (not courtside)")
ggplot(lower_results) +
  geom_histogram(aes(x = wSeat, y = ..density..), bins = 7) +
  stat_function(fun=dnorm,
                color="red",
                args=list(mean=mean(lower_results$wSeat, na.rm = TRUE), 
                          sd=sd(lower_results$wSeat, na.rm = TRUE))) +
  geom_vline(xintercept = mean(lower_results$wSeat, na.rm = TRUE), color = "blue") +
  geom_vline(xintercept = mean(lower_results$wSeat, na.rm = TRUE) + sd(lower_results$wSeat, na.rm = TRUE), color = "green") +
  geom_vline(xintercept = mean(lower_results$wSeat, na.rm = TRUE) - sd(lower_results$wSeat, na.rm = TRUE), color = "green") +
  labs(title = "Weighted Seat Satisfaction Distribution for Lower Level", x = "weighted seatsatisfaction") +
  theme_solarized() +
  NULL

upper_results <- results %>% filter(Seat_location == "Upper level")
ggplot(upper_results) +
  geom_histogram(aes(x = wSeat, y = ..density..), bins = 7) +
  stat_function(fun=dnorm,
                color="red",
                args=list(mean=mean(upper_results$wSeat, na.rm = TRUE), 
                          sd=sd(upper_results$wSeat, na.rm = TRUE))) +
  geom_vline(xintercept = mean(upper_results$wSeat, na.rm = TRUE), color = "blue") +
  geom_vline(xintercept = mean(upper_results$wSeat, na.rm = TRUE) + sd(upper_results$wSeat, na.rm = TRUE), color = "green") +
  geom_vline(xintercept = mean(upper_results$wSeat, na.rm = TRUE) - sd(upper_results$wSeat, na.rm = TRUE), color = "green") +
  labs(title = "Weighted Seat Satisfaction Distribution for Upper Level", x = "weighted seatsatisfaction") +
  theme_solarized() +
  NULL

courtside_results <- results %>% filter(Seat_location == "Courtside")
ggplot(courtside_results) +
  geom_histogram(aes(x = wSeat, y = ..density..), bins = 7) +
  stat_function(fun=dnorm,
                color="red",
                args=list(mean=mean(courtside_results$wSeat, na.rm = TRUE), 
                          sd=sd(courtside_results$wSeat, na.rm = TRUE))) +
  geom_vline(xintercept = mean(courtside_results$wSeat, na.rm = TRUE), color = "blue") +
  geom_vline(xintercept = mean(courtside_results$wSeat, na.rm = TRUE) + sd(courtside_results$wSeat, na.rm = TRUE), color = "green") +
  geom_vline(xintercept = mean(courtside_results$wSeat, na.rm = TRUE) - sd(courtside_results$wSeat, na.rm = TRUE), color = "green") +
  labs(title = "Weighted Seat Satisfaction Distribution for Courtside", x = "weighted seatsatisfaction") +
  theme_solarized() +
  NULL

club_results <- results %>% filter(Seat_location == "Club level")
ggplot(club_results) +
  geom_histogram(aes(x = wSeat, y = ..density..), bins = 5) +
  stat_function(fun=dnorm,
                color="red",
                args=list(mean=mean(club_results$wSeat, na.rm = TRUE), 
                          sd=sd(club_results$wSeat, na.rm = TRUE))) +
  geom_vline(xintercept = mean(club_results$wSeat, na.rm = TRUE), color = "blue") +
  geom_vline(xintercept = mean(club_results$wSeat, na.rm = TRUE) + sd(club_results$wSeat, na.rm = TRUE), color = "green") +
  geom_vline(xintercept = mean(club_results$wSeat, na.rm = TRUE) - sd(club_results$wSeat, na.rm = TRUE), color = "green") +
  labs(title = "Weighted Seat Satisfaction Distribution for Club Level", x = "weighted seatsatisfaction") +
  theme_solarized() +
  NULL
