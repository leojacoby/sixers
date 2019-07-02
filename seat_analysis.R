library("readxl")
library(tidyverse)
library(janitor)

results <- read_excel("data/game_survey.xlsx")

results <- remove_empty(results, which = c("cols"))
