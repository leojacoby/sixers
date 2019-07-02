library("readxl")
library(tidyverse)
library(janitor)

results <- read_excel("data/game_survey.xlsx")

results <- remove_empty(results, which = c("cols"))

## purpose vs. age
purpose <- results %>% select(7:12)
purpose <- na_if(purpose, '-')
names(purpose) <- c("Experience", "Business", "Fun", "general_passion", "visiting_passion", "Games")

purpose <- purpose %>% 
  mutate(
    choice1 = case_when(
      !is.na(Experience) ~ "Experience",
      !is.na(Business) ~ "Business",
      !is.na(Fun) ~ "Fun",
      !is.na(general_passion) ~ "NBA/Sixers passion",
      !is.na(visiting_passion) ~ "Passion for opposing team/player",
      TRUE ~ "unanswered"
    ),
    choice2 = case_when(
      !is.na(visiting_passion) ~ "Passion for opposing team/player",
      !is.na(general_passion) ~ "NBA/Sixers passion",
      !is.na(Fun) ~ "Fun",
      !is.na(Business) ~ "Business",
      TRUE ~ "unanswered"
    )
  ) %>% 
  filter(choice1 != "unanswered") %>% 
  select(choice1, choice2, Games) %>% 
  mutate(choice2 = ifelse(choice1 == choice2, NA, choice2))



choice1 <- purpose$choice1
choice2 <- purpose$choice2
choices <- c(choice1, choice2)
choices <- choices[!is.na(choices) & choices != "unanswered"]


ggplot(tibble(choices)) +
  geom_histogram(aes(x = choices, y = (..count..)/sum(..count..)), stat = "count") +
  labs(title = "Purpose for attending game", y = "density") +
  NULL

fun_purpose <- purpose %>% filter(choice1 == "Fun" | choice2 == "Fun")

choice1 <- fun_purpose$choice1
choice2 <- fun_purpose$choice2
choices <- c(choice1, choice2)

choices <- choices[!is.na(choices)]

choices <- choices[choices != "Fun" & choices != "NBA/Sixers passion"]

ggplot(data = tibble(choices), mapping = aes(x = choices)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  ggtitle("Other option for those who selected \"Fun\"") +
  ylab("density") +
  NULL



bball_purpose <- purpose %>% filter(choice1 == "NBA/Sixers passion" | choice2 == "NBA/Sixers passion")

choice1 <- bball_purpose$choice1
choice2 <- bball_purpose$choice2
choices <- c(choice1, choice2)

choices <- choices[!is.na(choices)]
choices <- choices[choices != "NBA/Sixers passion" & choices != "Fun"]

ggplot(data = tibble(choices), aes(x = choices)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  ggtitle("Other option for those who selected \"NBA/Sixers passion\"") +
  ylab("density") +
  NULL



choice1 <- purpose %>% select(choice = choice1, Games)
choice2 <- purpose %>% select(choice = choice2, Games)
single_choice_games <- rbind(choice1, choice2) %>% 
  filter(choice != "unanswered" & !is.na(Games)) %>% 
  mutate(Games = as.numeric(Games))

game_dist <- function(iso_choice) {
  iso_choice_games <- single_choice_games %>% filter(choice == iso_choice)
  ggplot(data = iso_choice_games) +
    geom_histogram(mapping = aes(x = Games, y = ..density..), binwidth = 5) +
    stat_function(fun=dnorm,
                  color="red",
                  args=list(mean=mean(iso_choice_games$Games), 
                            sd=sd(iso_choice_games$Games))) +
    geom_vline(xintercept = mean(iso_choice_games$Games), color = "blue") +
    geom_vline(xintercept = mean(iso_choice_games$Games) + sd(iso_choice_games$Games), color = "green") +
    geom_vline(xintercept = mean(iso_choice_games$Games) - sd(iso_choice_games$Games), color = "green") +
    labs(title = paste("Number of games attended for those who come for", iso_choice)) +
    NULL
}

game_dist("Fun")
game_dist("NBA/Sixers passion")
game_dist("Experience")
game_dist("Business")

iso_choice_games <- single_choice_games ##%>% filter(choice == "Fun")
ggplot(data = iso_choice_games) +
  geom_histogram(aes(Games, ..density..), bins = 9) +
  stat_function(fun=dnorm,
                color="red",
                args=list(mean=mean(iso_choice_games$Games), 
                          sd=sd(iso_choice_games$Games))) +
  geom_vline(xintercept = mean(iso_choice_games$Games), color = "blue") +
  geom_vline(xintercept = mean(iso_choice_games$Games) + sd(iso_choice_games$Games), color = "green") +
  geom_vline(xintercept = mean(iso_choice_games$Games) - sd(iso_choice_games$Games), color = "green") +
  labs(title = paste("Number of games attended for those who come for", "Fun")) +
  NULL

# purpose_facet_summary <- tribble(~choice, ~avg, ~stdev,
#                                  "Fun", 
#                                  mean(filter(single_choice_games, choice == "Fun")$Games),
#                                  sd(filter(single_choice_games, choice == "Fun")$Games),
#                                  "Experience", 
#                                  mean(filter(single_choice_games, choice == "Experience")$Games),
#                                  sd(filter(single_choice_games, choice == "Experience")$Games),
#                                  "Business", 
#                                  mean(filter(single_choice_games, choice == "Business")$Games),
#                                  sd(filter(single_choice_games, choice == "Business")$Games),
#                                  "NBA/Sixers passion", 
#                                  mean(filter(single_choice_games, choice == "NBA/Sixers passion")$Games),
#                                  sd(filter(single_choice_games, choice == "NBA/Sixers passion")$Games)
#                                  )


ggplot(data = filter(single_choice_games, choice != "Passion for opposing team/player"), mapping = aes(x = Games, y = ..density..)) +
  geom_histogram(binwidth = 5) +
  # stat_function(fun=dnorm,
  #               color="red",
  #               args=list(mean=mean(Games), 
  #                         sd=sd(Games))) +
  geom_vline(data = purpose_facet_summary, aes(xintercept = avg), color = "blue") +
  geom_vline(data = purpose_facet_summary, aes(xintercept = avg + stdev), color = "green") +
  geom_vline(data = purpose_facet_summary, aes(xintercept = avg - stdev), color = "green") +
  stat_function(
                mapping = aes(args = list(mean=game_mean,
                                          sd=game_sd)),
                fun=dnorm,
                color="red") +
  facet_wrap(~ choice, nrow = 2) +
  ggtitle("Number of Games attended broken down by reason for attending") +
  NULL




