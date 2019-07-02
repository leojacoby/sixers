library("readxl")
library(tidyverse)
library(janitor)

results <- read_excel("data/game_survey.xlsx")

results <- remove_empty(results, which = c("cols"))

## purpose vs. age
purpose <- results %>% select(5, 7:11)
purpose <- na_if(purpose, '-')
names(purpose) <- c("Age", "Experience", "Business", "Fun", "general_passion", "visiting_passion")

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
  select(Age, choice1, choice2) %>% 
  mutate(choice2 = ifelse(choice1 == choice2, NA, choice2))

choice1_tbl <- purpose %>% select(Age, choice1) %>% rename(choice = choice1)
choice2_tbl <- purpose %>% select(Age, choice2) %>% rename(choice = choice2)

purpose <- rbind(choice1_tbl, choice2_tbl) %>% 
  filter(choice != "unanswered")


## would love to be able to see clearer splits for less populated age ranges and categories

ggplot(data = purpose) +
  geom_bin2d(aes(x = Age, y = choice)) +
  scale_fill_distiller(palette = "Spectral") +
  ggtitle("Breakdown of fan's reason for attending games by age")
  NULL

ggplot(data = purpose) +
  geom_bar(aes(x = Age), fill = "blue") +
  ggtitle("Age distribution of survey responders") +
  NULL
## age vs satisfaction in different aspects of stadium experience
## age, mean satisfaction, stadium experience
  
satisfaction <- results %>% select(starts_with("14."))
names(satisfaction) <- c("Transportation", "Enter_exit", "Phone_tech", "Food", "Retail", "Entertainment", "Seat")
satisfaction[satisfaction == "7 - Extremely satisfied"] <- 7
satisfaction[satisfaction == "1 - Not at all satisfied"] <- 1
satisfaction <- satisfaction %>% 
  filter(!(is.na(Transportation) & is.na(Enter_exit) & is.na(Phone_tech) & is.na(Food) & is.na(Retail) & is.na(Entertainment) & is.na(Seat)))


numericize <- function(x) {
  as.numeric(x)
}

satisfaction <- satisfaction %>% 
  mutate_all(numericize)

transportation <- as_tibble(satisfaction$Transportation) %>% 
  mutate(category = "Transportation")
enter_exit <- as_tibble(satisfaction$Enter_exit) %>% 
  mutate(category = "Enter_exit")
phone_tech <- as_tibble(satisfaction$Phone_tech) %>% 
  mutate(category = "Phone_tech")
food <- as_tibble(satisfaction$Food) %>% 
  mutate(category = "Food")
retail <- as_tibble(satisfaction$Retail) %>% 
  mutate(category = "Retail")
entertainment <- as_tibble(satisfaction$Entertainment) %>% 
  mutate(category = "Entertainment")
seat <- as_tibble(satisfaction$Seat) %>% 
  mutate(category = "Seat")

satisfaction <- rbind(transportation, enter_exit, phone_tech, food, retail, entertainment, seat)

satisfaction_hist <- function(x, tit) {
  ggplot(filter(satisfaction, category == x), aes(x = value)) +
    geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue") +
    xlab("rating") +
    ggtitle(paste(tit, "satisfaction (1-7)")) +
    geom_vline(xintercept = mean(filter(satisfaction, category == x)$value)) +
    NULL
}

satisfaction_hist("Transportation", "Transportation to and from arena")
satisfaction_hist("Enter_exit", "Entering and exiting arena")
satisfaction_hist("Phone_tech", "Arena tech")
satisfaction_hist("Food", "Food & beverage")
satisfaction_hist("Retail", "Arena retail")
satisfaction_hist("Entertainment", "Game entertainment")
satisfaction_hist("Seat", "Seat experience")

player_video_satisfaction <- results[["35. [Topic:Pre-recorded feature about a player (e.g., showcasing a player's personality / interests); Category:A; Question:Please rate your satisfaction with the following in-game entertainment elements (if you do not recall any one of the elements listed below, please mark N/A).]"]]
player_video_satisfaction[player_video_satisfaction == "7 - Extremely satisfied"] <- 7
player_video_satisfaction[player_video_satisfaction == "1 - Not satisfied at all"] <- 1
player_video_satisfaction[player_video_satisfaction == "NA"] <- NA

player_video_satisfaction <- player_video_satisfaction[!is.na(player_video_satisfaction)]
player_video_satisfaction <- as.numeric(player_video_satisfaction)

ggplot(data = tibble(player_video_satisfaction), aes(x = player_video_satisfaction, y = ..density..)) +
  geom_histogram(binwidth = 1, fill = "blue") +
  geom_vline(xintercept = mean(player_video_satisfaction)) +
  labs(x = "rating", title = "Player personality video satisfaction")

player_community_satisfaction <- results[["35. [Topic:Video of players at community events (e.g., a charity night, volunteer efforts); Category:A; Question:Please rate your satisfaction with the following in-game entertainment elements (if you do not recall any one of the elements listed below, please mark N/A).]"]]
player_community_satisfaction[player_community_satisfaction == "7 - Extremely satisfied"] <- 7
player_community_satisfaction[player_community_satisfaction == "1 - Not satisfied at all"] <- 1
player_community_satisfaction[player_community_satisfaction == "NA"] <- NA

player_community_satisfaction <- player_community_satisfaction[!is.na(player_community_satisfaction)]
player_community_satisfaction <- as.numeric(player_community_satisfaction)

ggplot(data = tibble(player_community_satisfaction), aes(x = player_community_satisfaction, y = ..density..)) +
  geom_histogram(binwidth = 1, fill = "blue") +
  geom_vline(xintercept = mean(player_community_satisfaction)) +
  labs(x = "rating", title = "Players in community video satisfaction")


##ideas
## food breakdown (price, speed, selection, etc.)


