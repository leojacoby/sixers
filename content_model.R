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
    Content_app_freq = "105. [Topic:Viewing team content; Category:A; Question:How often do you use the team mobile app for each of the following?]",
    App_download = "80. Have you downloaded and used your team's mobile app on your smartphone?"
  ) %>% 
  filter(!is.na(Games) & Games > 0)

results <- results %>% 
  filter(!is.na(App_download)) %>% 
  mutate(Games = as.numeric(Games),
         Content_app_freq = ifelse(
           !(grepl("Frequently", Content_app_freq) | grepl("Sometimes", Content_app_freq)), 
           "Never", 
           Content_app_freq))

results <- results %>% 
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

results$Content_app_freq <- factor(results$Content_app_freq, 
                                  ordered = TRUE,
                                  levels = c("Never", "Sometimes", "Frequently"))

content_model <- polr(formula = Content_app_freq ~ Age + Games, data = results, Hess = TRUE)

ctable <- coef(summary(content_model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p_value" = p)
