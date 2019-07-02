library("readxl")
library(tidyverse)
library(janitor)

results <- read_excel("data/game_survey.xlsx")

results <- remove_empty(results, which = c("cols"))

results <- results %>% 
  select(
    Age = "71. What is your current age?",
    Level = "131. In which part of the arena do you most frequently purchase seats?",
    Enter_exit_satisfaction = "14. [Topic:Entering and exiting the arena (Will Call, security, ticket takers); Category:A; Question:How satisfied are you with each of the following experiences when attending a game?]",
    Seat_comfort_satisfaction = "14. [Topic:In-seat experience (seat location, seat comfort, service from ushers, hawkers); Category:A; Question:How satisfied are you with each of the following experiences when attending a game?]",
    Seat_importance = "15. [Topic:In-seat experience (seat location, seat comfort, service from ushers, hawkers); Category:A; Question:Please rank the following experiences in order of their importance to your overall satisfaction with the game.]",
    Time_to_seat_satisfaction = "133. [Topic:Time to seat; Category:A; Question:How satisfied are you with the time it takes to get from the arena entrance (e.g. security line) to your seat?]",
    Tshirt_toss_satisfaction = "35. [Topic:T-shirt toss into the crowd; Category:A; Question:Please rate your satisfaction with the following in-game entertainment elements (if you do not recall any one of the elements listed below, please mark N/A).]",
    Sound_satisfaction = "135. [Topic:Audio levels; Category:A; Question:Please rate your overall impression of the audio levels/volume of the speakers in the arena.]",
    Hawker_satisfaction = "36. [Topic:Service from hawkers (e.g. selling and delivering food and/or beverage to seat); Category:A; Question:Please rate your satisfaction with the following in-seat experience elements.]"
         ) %>% 
  filter(!is.na(Age))

results[results == "7 - Extremely satisfied"] <- "7"
results[results == "1 - Not at all satisfied"] <- "1"
results[results == "1 - Not satisfied at all"] <- "1"

results[results == "NA"] <- NA


results <- results %>% 
  mutate(Enter_exit_satisfaction = as.numeric(Enter_exit_satisfaction)) %>% 
  mutate(Seat_comfort_satisfaction = as.numeric(Seat_comfort_satisfaction)) %>% 
  mutate(Seat_importance = as.numeric(Seat_importance)) %>% 
  mutate(Time_to_seat_satisfaction = as.numeric(Time_to_seat_satisfaction)) %>% 
  mutate(Tshirt_toss_satisfaction = as.numeric(Tshirt_toss_satisfaction)) %>% 
  mutate(Hawker_satisfaction = as.numeric(Hawker_satisfaction))
  
  

lower_level <- results %>% filter(Level == "Lower level (not courtside)", !is.na(Seat_comfort_satisfaction))
ggplot(data = lower_level) + 
  geom_histogram(aes(x = Seat_comfort_satisfaction), stat = "count", binwidth = 1) +
  geom_vline(xintercept = mean(lower_level$Seat_comfort_satisfaction)) +
  labs(x = "Lower level seat comfort satisfaction")

upper_level <- results %>% filter(Level == "Upper level", !is.na(Seat_comfort_satisfaction))
ggplot(data = upper_level) + 
  geom_histogram(aes(x = Seat_comfort_satisfaction), stat = "count", binwidth = 1) +
  geom_vline(xintercept = mean(upper_level$Seat_comfort_satisfaction)) +
  labs(x = "Upper level seat comfort satisfaction")




lower_level <- results %>% filter(Level == "Lower level (not courtside)", !is.na(Tshirt_toss_satisfaction))
ggplot(data = lower_level) + 
  geom_histogram(aes(x = Tshirt_toss_satisfaction), stat = "count", binwidth = 1) +
  geom_vline(xintercept = mean(lower_level$Tshirt_toss_satisfaction)) +
  labs(x = "Lower level tshirt toss satisfaction")

upper_level <- results %>% filter(Level == "Upper level", !is.na(Tshirt_toss_satisfaction))
ggplot(data = upper_level) + 
  geom_histogram(aes(x = Tshirt_toss_satisfaction), stat = "count", binwidth = 1) +
  geom_vline(xintercept = mean(upper_level$Tshirt_toss_satisfaction)) +
  labs(x = "Upper level tshirt toss satisfaction")




lower_level <- results %>% filter(Level == "Lower level (not courtside)", !is.na(Sound_satisfaction) & Sound_satisfaction != "Too soft")
ggplot(data = lower_level) + 
  geom_bar(aes(x = Sound_satisfaction)) +
  labs(x = "Lower level arena audio satisfaction")

upper_level <- results %>% filter(Level == "Upper level", !is.na(Sound_satisfaction))
ggplot(data = upper_level) + 
  geom_bar(aes(x = Sound_satisfaction)) +
  labs(x = "Upper level arena audio satisfaction")




lower_level <- results %>% filter(Level == "Lower level (not courtside)", !is.na(Price_satisfaction))
ggplot(data = lower_level) + 
  geom_histogram(aes(x = Price_satisfaction), stat = "count", binwidth = 1) +
  geom_vline(xintercept = mean(lower_level$Price_satisfaction)) +
  labs(x = "Lower level food price satisfaction")

upper_level <- results %>% filter(Level == "Upper level", !is.na(Price_satisfaction))
ggplot(data = upper_level) + 
  geom_histogram(aes(x = Price_satisfaction), stat = "count", binwidth = 1) +
  geom_vline(xintercept = mean(upper_level$Price_satisfaction)) +
  labs(x = "Upper level food price satisfaction")

sound_results <- results %>% 
  filter(!is.na(Sound_satisfaction)) %>% 
  group_by(Age, Sound_satisfaction) %>% 
  add_tally() %>% 
  ungroup() %>% 
  select(Age, Sound_satisfaction, n) %>% 
  distinct() %>% 
  group_by(Age) %>% 
  mutate(age_total = sum(n)) %>% 
  filter(Sound_satisfaction == "Too loud") %>% 
  mutate(too_loud_percent = n/age_total)

ggplot(data = sound_results) +
  geom_point(aes(x = Age, y = too_loud_percent)) +
  geom_abline(intercept = fit$coefficients[1], slope = fit$coefficients[2], color = "red") +
  labs(y = "Percent of fans who think audio is too loud", title = "Fan age vs. opinion on arena audio") +
  NULL
