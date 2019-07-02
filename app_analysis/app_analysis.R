library("readxl")
library(tidyverse)
library(janitor)
library(modelr)
library(MASS)
library(plotly)
library(ggthemes)


results <- read_excel("data/game_survey.xlsx")

results <- remove_empty(results, which = c("cols"))

results <- results %>% 
  dplyr::select(
    Age = "71. What is your current age?",
    Games = "2. Approximately how many regular season %[72]Q5LBL% games have you attended during the 2018-19 season?",
    App_use = "78. Engaging the team/arena mobile app (e.g. team content, mobile upgrades) [Question: For which of the following applications do you use your smartphone when at a game?]",
    App_satisfaction = "81. [Topic:Mobile app; Category:A; Question:How satisfied are you with your team's mobile app?]",
    App_download = "80. Have you downloaded and used your team's mobile app on your smartphone?",
    Ticket_app_freq = "105. [Topic:Ticketing; Category:A; Question:How often do you use the team mobile app for each of the following?]",
    Upgrade_app_freq = "105. [Topic:Seat upgrades; Category:A; Question:How often do you use the team mobile app for each of the following?]",
    FB_app_freq = "105. [Topic:F&B seat ordering; Category:A; Question:How often do you use the team mobile app for each of the following?]",
    Merch_app_freq = "105. [Topic:Purchasing merchandise; Category:A; Question:How often do you use the team mobile app for each of the following?]",
    Content_app_freq = "105. [Topic:Viewing team content; Category:A; Question:How often do you use the team mobile app for each of the following?]"
  ) %>% 
  filter(!is.na(Games) & Games > 0) %>% 
  mutate(App_use = ifelse(grepl("Engaging", App_use), TRUE, FALSE),
         App_download = case_when(
           grepl("Yes", App_download) ~ 1,
           grepl("No", App_download) ~ 0))
  
results[results == "7 - Extremely satisfied"] <- "7"
results[results == "1 - Not at all satisfied"] <- "1"
results[results == "1 - Not satisfied at all"] <- "1"
results[results == "NA"] <- NA

results <- results %>% 
  mutate(App_satisfaction = as.numeric(App_satisfaction),
         Games = as.numeric(Games))

results <- results %>% 
  filter(!is.na(App_download)) %>% 
  mutate(Ticket_app_freq = ifelse(!(grepl("Frequently", Ticket_app_freq) | grepl("Sometimes", Ticket_app_freq)), "Never", Ticket_app_freq)) %>% 
  mutate(Games_bin = case_when(
            Games > 22 ~ "23+",
            Games >= 14 ~ "14-22",
            Games >= 9 ~ "9-13",
            Games > 0 ~ "1-8"
          )) %>% 
  group_by(Games_bin) %>% 
  add_tally() %>% 
  rename(total = n) %>% 
  ungroup()

downloads <- results %>% 
  group_by(Games_bin, App_download) %>% 
  add_tally() %>% 
  ungroup() %>% 
  rename(n_downloads = n) %>% 
  filter(App_download == TRUE) %>% 
  select(Games_bin, n_downloads, total) %>% 
  distinct() %>% 
  mutate(download_pct = n_downloads/total) %>% 
  select(Games_bin, download_pct) %>% 
  distinct()


ggplot(downloads, mapping = aes(x = Games_bin, y = download_pct)) +
  geom_bar(stat = "identity") + 
  scale_x_discrete(limits=c("1-8", "9-13", "14-22", "23+")) +
  theme_classic() +
  labs(title = "Games attended vs. Use of Sixers app", x = "Games attended", y = "% of fans who downloaded and used") +
  NULL


games_ticketing <- results %>% 
  group_by(Games_bin, Ticket_app_freq) %>% 
  add_tally() %>% 
  ungroup() %>% 
  rename(n_freq = n) %>% 
  filter(Ticket_app_freq != "Never") %>% 
  select(Games_bin, Ticket_app_freq, n_freq, total) %>% 
  distinct() %>% 
  mutate(freq_pct = n_freq/total)

ggplot(games_ticketing, aes(x = Games_bin, y = freq_pct, fill = Ticket_app_freq)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  scale_x_discrete(limits=c("1-8", "9-13", "14-22", "23+")) +
  labs(title = "Sixers app use for ticketing by games attended", fill = "Frequency of use", y = "% of respondents", x = "Games attended") +
  theme_classic()

age_ticketing <- results %>% 
  group_by(Age, Ticket_app_freq) %>% 
  add_tally() %>% 
  ungroup() %>% 
  rename(n_freq = n) %>% 
  filter(Ticket_app_freq != "Never") %>% 
  dplyr::select(Age, Ticket_app_freq, n_freq, total) %>% 
  distinct() %>% 
  mutate(freq_pct = n_freq/total)

ggplot(age_ticketing, aes(x = Age, y = freq_pct, fill = Ticket_app_freq)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  labs(title = "Sixers app use for ticketing by age", fill = "Frequency of use", y = "% of respondents", x = "Age range") +
  theme_classic()

ovo <- results %>% 
  mutate(Age = case_when(
    Age == "18-24" ~ 21,
    Age == "25-29" ~ 27,
    Age == "30-34" ~ 32,
    Age == "35-39" ~ 37,
    Age == "40-44" ~ 42,
    Age == "45-49" ~ 47,
    Age == "50-54" ~ 52,
    Age == "55-59" ~ 57,
    Age == "60-64" ~ 62,
    Age == "65-69" ~ 67,
    Age == "70 or older" ~ 74
  ))

ggplot(ovo, aes(x = Age, y = Games)) +
  geom_point(alpha = 0.2) +
  stat_smooth(method = "lm", se = TRUE) +
  labs(y = "Games attended in 2019-20", title = "The effect of age on game attendence") +
  theme_gray()

age_game_model <- lm(Games ~ Age, data = ovo)

## Age, Games, model

model_data <- results %>% 
  mutate(Age = case_when(
    Age == "18-24" ~ 21,
    Age == "25-29" ~ 27,
    Age == "30-34" ~ 32,
    Age == "35-39" ~ 37,
    Age == "40-44" ~ 42,
    Age == "45-49" ~ 47,
    Age == "50-54" ~ 52,
    Age == "55-59" ~ 57,
    Age == "60-64" ~ 62,
    Age == "65-69" ~ 67,
    Age == "70 or older" ~ 74
  ),
  App_download = ifelse(is.na(App_download), FALSE, App_download))
  
download_model <- glm(App_download ~ Games, data = model_data, family = binomial)
model_data$prob <- predict(download_model, model_data, type = "response")
ggplot(data = model_data, aes(Games, App_download)) +
  stat_smooth(method = "glm", method.args = list(family = binomial), se = TRUE) +
  labs(title = "App download probability using games attended", 
       x = "Games attended", 
       y = "Probability of app download") +
  NULL

ticketing_model_data <- model_data

ticketing_model_data$Ticket_app_freq <- factor(ticketing_model_data$Ticket_app_freq, 
                                               ordered = TRUE,
                                               levels = c("Never", "Sometimes", "Frequently"))

ticketing_model <- polr(formula = Ticket_app_freq ~ Age + Games, data = ticketing_model_data, Hess = TRUE)

## this is wack bc it says that games has a negative coefficienet when earlier my rough graph looked pretty positivie
## either I have the factor backwards or Age correlates with more Games so the older you are the more games you go to and
## the more you use the app.

