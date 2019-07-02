library(tidyverse)
library(magrittr)
library(lubridate)
library(plotly)
setwd("/Users/leojacoby/programming/R/Sixers")

data <- read_csv("data/stubhub_playoff_listings.csv")


###


seat_data <- data %>% 
  select(-currentPrice, -score, -sectionId, -zoneId) %>% 
  filter(!grepl("park", tolower(row)) & !grepl("park", tolower(Event)) & !grepl("park", tolower(sectionName))) %>% 
  mutate(Season = ifelse(Season == "SI2017", "2019", "2018")) %>% 
  mutate(Round = case_when(
    grepl("First Round", Event) ~ 1,
    grepl("Semifinals", Event) ~ 2,
    grepl("Eastern Conference Finals", Event) ~ 3,
    grepl("NBA Finals", Event) ~ 4
  )) %>% 
  mutate(Home_game_num = as.numeric(str_sub(str_extract(Event, "Home Game \\d"), -1))) %>% 
  select(-Event) %>% 
  mutate(zoneName = ifelse(zoneName == "Risers" | zoneName == "Centennial South", "Lower Baseline", zoneName),
         zoneName = ifelse(grepl("South", zoneName) | grepl("North", zoneName), str_replace_all(tolower(zoneName), "south|north", "baseline"), zoneName),
         zoneName = ifelse(grepl("East", zoneName) | grepl("West", zoneName), str_replace_all(tolower(zoneName), "east|west", "sideline"), zoneName),
         level = case_when(
           grepl("lower", tolower(zoneName)) ~ "lower",
           grepl("upper", tolower(zoneName)) ~ "upper",
           grepl("floor", tolower(zoneName)) ~ "floor/courtside",
           grepl("courtside", tolower(zoneName)) ~ "floor/courtside",
           grepl("box", tolower(zoneName)) ~ "club",
           grepl("club", tolower(zoneName)) ~ "club",
           grepl("suite", tolower(zoneName)) ~ "suite",
           is.na(zoneName) ~ case_when(
             grepl("courtside", tolower(sectionName)) ~ "floor/courtside",
             grepl("suite", tolower(sectionName)) ~ "suite",
             !is.na(str_extract(sectionName, "1\\d\\d")) ~ "lower",
             !is.na(str_extract(sectionName, "2\\d\\d")) ~ "upper"
           )
         ),
         ticketPrice = as.numeric(listingPrice),
         listingPrice = as.numeric(listingPrice) * as.numeric(quantity),
         retrieveTime = mdy_hm(retrieveTime)) %>% 
  filter(ticketPrice < 25000)

seat_data_2019 <- seat_data %>% 
  filter(Season == "2019") %>% 
  mutate(game_date = case_when(
    (Round == 1 & Home_game_num == 1) ~ ymd_hm("2019-04-13 7:00"),
    (Round == 1 & Home_game_num == 2) ~ ymd_hm("2019-04-15 7:00"),
    (Round == 1 & Home_game_num == 3) ~ ymd_hm("2019-04-23 7:00"),
    (Round == 1 & Home_game_num == 4) ~ ymd_hm("2019-04-27 7:00"),
    (Round == 2 & Home_game_num == 1) ~ ymd_hm("2019-05-02 7:00"),
    (Round == 2 & Home_game_num == 2) ~ ymd_hm("2019-05-05 7:00"),
    (Round == 2 & Home_game_num == 3) ~ ymd_hm("2019-05-09 7:00"),
    (Round == 2 & Home_game_num == 4) ~ ymd_hm("2019-05-12 7:00"),
    (Round == 3 & Home_game_num == 1) ~ ymd_hm("2019-05-19 7:00"),
    (Round == 3 & Home_game_num == 2) ~ ymd_hm("2019-05-21 7:00"),
    (Round == 3 & Home_game_num == 3) ~ ymd_hm("2019-05-25 7:00"),
    (Round == 3 & Home_game_num == 4) ~ ymd_hm("2019-05-27 7:00"),
    (Round == 4 & Home_game_num == 1) ~ ymd_hm("2019-06-05 7:00"),
    (Round == 4 & Home_game_num == 2) ~ ymd_hm("2019-06-07 7:00"),
    (Round == 4 & Home_game_num == 3) ~ ymd_hm("2019-06-13 7:00"),
    (Round == 4 & Home_game_num == 4) ~ ymd_hm("2019-06-16 7:00")
  ),
  daysBeforeGame = as.numeric(game_date - retrieveTime)/24)

seat_data_2018 <- seat_data %>% 
  filter(Season == "2018") %>% 
  mutate(game_date = case_when(
    (Round == 1 & Home_game_num == 1) ~ ymd_hm("2018-04-14 7:00"),
    (Round == 1 & Home_game_num == 2) ~ ymd_hm("2018-04-16 7:00"),
    (Round == 1 & Home_game_num == 3) ~ ymd_hm("2018-04-24 7:00"),
    (Round == 1 & Home_game_num == 4) ~ ymd_hm("2018-04-29 7:00"),
    (Round == 2 & Home_game_num == 1) ~ ymd_hm("2018-05-05 7:00"),
    (Round == 2 & Home_game_num == 2) ~ ymd_hm("2018-05-07 7:00"),
    (Round == 2 & Home_game_num == 3) ~ ymd_hm("2018-05-11 7:00"),
    (Round == 2 & Home_game_num == 4) ~ ymd_hm("2018-05-13 7:00"),
    (Round == 3 & Home_game_num == 1) ~ ymd_hm("2018-05-13 7:00"),
    (Round == 3 & Home_game_num == 2) ~ ymd_hm("2018-05-15 7:00"),
    (Round == 3 & Home_game_num == 3) ~ ymd_hm("2018-05-23 7:00"),
    (Round == 3 & Home_game_num == 4) ~ ymd_hm("2018-05-27 7:00"),
    (Round == 4 & Home_game_num == 1) ~ ymd_hm("2018-06-06 7:00"),
    (Round == 4 & Home_game_num == 2) ~ ymd_hm("2018-06-08 7:00"),
    (Round == 4 & Home_game_num == 3) ~ ymd_hm("2018-06-13 7:00"),
    (Round == 4 & Home_game_num == 4) ~ ymd_hm("2018-06-15 7:00")
  ),
  daysBeforeGame = as.numeric(game_date - retrieveTime)/24)

seat_data <- rbind(seat_data_2018, seat_data_2019) 
#   mutate(Season = toString(Season))

oldest_listing_seat_data <- seat_data %>% 
  group_by(listingId) %>% 
  filter(retrieveTime == min(retrieveTime))

ggplot(oldest_listing_seat_data, aes(x = daysBeforeGame, fill = Season)) +
  geom_density(alpha = 0.2) +
  labs(x = "Days before game", title = "Stubhub listing time distribution by year") +
  NULL

ggplot(oldest_listing_seat_data) +
  geom_bar(aes(x = Season))

game_labeller <- function(variable, value) {
  series_game <- case_when(
    value == 1 ~ 3,
    value == 2 ~ 4,
    value == 3 ~ 6
  )
  return(paste("Game", series_game))
}

game_labeller2 <- function(variable, value) {
  series_game <- case_when(
    value == 1 ~ 1,
    value == 2 ~ 2,
    value == 3 ~ 5
  )
  return(paste("Game", series_game))
}

######

nets_series <- seat_data %>% 
  filter(Season == "2019" & Round == 1 & Home_game_num != 4) %>% 
  group_by(retrieveTime, Home_game_num, date) %>% 
  summarize(sum = sum(quantity), avg_price = mean(ticketPrice))

ggplot(nets_series, aes(retrieveTime, sum)) + 
  geom_point() +
  geom_vline(aes(xintercept = date), color = "red") +
  labs(x = "Time", y = "seats", title = "Seats on Stubhub for home games vs. Nets") +
  ylim(0, 6000) + 
  facet_wrap(~ Home_game_num, nrow = 3, labeller = game_labeller2)

nets_vlines <- c(
  ymd_hm("2019-04-13 7:00"),
  ymd_hm("2019-04-15 7:00"),
  ymd_hm("2019-04-18 7:00"),
  ymd_hm("2019-04-20 7:00"),
  ymd_hm("2019-04-23 7:00")
)

nets_vline_table <- tibble(
  vline1_xint = c(rep(NA, 0), rep(nets_vlines[1], 3)),
  vline2_xint = c(rep(NA, 1), rep(nets_vlines[2], 2)),
  vline3_xint = c(rep(NA, 2), rep(nets_vlines[3], 1)),
  vline4_xint = c(rep(NA , 2), rep(nets_vlines[4], 1)),
  vline5_xint = c(rep(NA , 2), rep(nets_vlines[5], 1)),
  Home_game_num = c(1, 2, 3)
)

ggplot(nets_series, aes(x = retrieveTime, y = sum)) + 
  geom_point() +
  geom_vline(data = nets_vline_table, aes(xintercept = vline1_xint), color = "red") +
  geom_vline(data = nets_vline_table, aes(xintercept = vline2_xint), color = "green") +
  geom_vline(data = nets_vline_table, aes(xintercept = vline3_xint), color = "green", linetype = "dashed") +
  geom_vline(data = nets_vline_table, aes(xintercept = vline4_xint), color = "green", linetype = "dashed") +
  geom_vline(data = nets_vline_table, aes(xintercept = vline5_xint), color = "green") +
  labs(x = "Time", y = "seats", title = "Seats available on Stubhub during 2019 First Round") +
  ylim(0, 6000) +
  facet_wrap(~Home_game_num, nrow = 3, labeller = game_labeller2) + 
  theme_gray() +
  NULL


raptors_series <- seat_data %>% 
  filter(Season == "2019" & Round == 2 & Home_game_num != 4) %>% 
  group_by(retrieveTime, Home_game_num, date) %>% 
  summarize(sum = sum(quantity), avg_price = mean(ticketPrice))


ggplot(raptors_series, aes(retrieveTime, sum)) + 
  geom_point() +
  geom_vline(aes(xintercept = date), color = "red") +
  labs(x = "Time", y = "seats", title = "Seats on Stubhub for home games vs. Raptors") +
  ylim(0, 6000) + 
  facet_wrap(~ Home_game_num, nrow = 3, labeller = game_labeller)

raptors_vlines <- c(
  ymd_hm("2019-04-27 7:00"),
  ymd_hm("2019-04-29 7:00"),
  ymd_hm("2019-05-02 7:00"),
  ymd_hm("2019-05-05 7:00"),
  ymd_hm("2019-05-07 7:00"),
  ymd_hm("2019-05-09 7:00")
)

raptors_vline_table <- tibble(
  vline1_xint = c(rep(NA, 0), rep(raptors_vlines[1], 3)),
  vline2_xint = c(rep(NA, 0), rep(raptors_vlines[2], 3)),
  vline3_xint = c(rep(NA, 0), rep(raptors_vlines[3], 3)),
  vline4_xint = c(rep(NA , 1), rep(raptors_vlines[4], 2)),
  vline5_xint = c(rep(NA , 2), rep(raptors_vlines[5], 1)),
  vline6_xint = c(rep(NA , 2), rep(raptors_vlines[6], 1)),
  vline1_color = c(rep(NA, 0), rep("red", 3)),
  vline2_color = c(rep(NA, 0), rep("green", 3)),
  vline3_color = c(rep(NA, 0), rep("green", 3)),
  vline4_color = c(rep(NA, 1), rep("red", 2)),
  vline5_color = c(rep(NA, 2), rep("red", 1)),
  vline6_color = c(rep(NA, 2), rep("green", 1)),
  vline1_line = c(rep(NA, 0), rep("dashed", 3)),
  vline2_line = c(rep(NA, 0), rep("dashed", 3)),
  vline3_line = c(rep(NA, 0), rep("solid", 3)),
  vline4_line = c(rep(NA, 1), rep("solid", 2)),
  vline5_line = c(rep(NA, 2), rep("dashed", 1)),
  vline6_line = c(rep(NA, 2), rep("solid", 1)),
  Home_game_num = c(1, 2, 3)
  )

ggplot(raptors_series, aes(x = retrieveTime, y = sum)) + 
  geom_point() +
  geom_vline(data = raptors_vline_table, aes(xintercept = vline1_xint), color = "red", linetype = "dashed") +
  geom_vline(data = raptors_vline_table, aes(xintercept = vline2_xint), color = "green", linetype = "dashed") +
  geom_vline(data = raptors_vline_table, aes(xintercept = vline3_xint), color = "green") +
  geom_vline(data = raptors_vline_table, aes(xintercept = vline4_xint), color = "red") +
  geom_vline(data = raptors_vline_table, aes(xintercept = vline5_xint), color = "red", linetype = "dashed") +
  geom_vline(data = raptors_vline_table, aes(xintercept = vline6_xint), color = "green") +
  labs(x = "Time", y = "seats", title = "Seats available on Stubhub during 2019 Conference Semifinals") +
  ylim(0, 6000) +
  facet_wrap(~Home_game_num, nrow = 3, labeller = game_labeller) + 
  theme_gray()
  NULL


heat_series <- seat_data %>% 
  filter(Season == "2018" & Round == 1 & Home_game_num != 4) %>% 
  group_by(retrieveTime, Round, Home_game_num, date) %>% 
  summarize(sum = sum(quantity), avg_price = mean(ticketPrice))

ggplot(heat_series, aes(retrieveTime, sum)) + 
  geom_point() +
  geom_vline(aes(xintercept = date), color = "red") +
  labs(x = "Time", y = "seats", title = "Seats on Stubhub for home games vs. Heat") +  
  ylim(0, 6000) + 
  facet_wrap(~ Home_game_num, nrow = 3, labeller = game_labeller2)

heat_vlines <- c(
  ymd_hm("2018-04-14 7:00"),
  ymd_hm("2018-04-16 7:00"),
  ymd_hm("2018-04-19 7:00"),
  ymd_hm("2018-04-21 7:00"),
  ymd_hm("2018-04-24 7:00")
)

heat_vline_table <- tibble(
  vline1_xint = c(rep(NA, 0), rep(heat_vlines[1], 3)),
  vline2_xint = c(rep(NA, 1), rep(heat_vlines[2], 2)),
  vline3_xint = c(rep(NA, 2), rep(heat_vlines[3], 1)),
  vline4_xint = c(rep(NA , 2), rep(heat_vlines[4], 1)),
  vline5_xint = c(rep(NA , 2), rep(heat_vlines[5], 1)),
  Home_game_num = c(1, 2, 3)
)

ggplot(heat_series, aes(x = retrieveTime, y = sum)) + 
  geom_point() +
  geom_vline(data = heat_vline_table, aes(xintercept = vline1_xint), color = "green") +
  geom_vline(data = heat_vline_table, aes(xintercept = vline2_xint), color = "red") +
  geom_vline(data = heat_vline_table, aes(xintercept = vline3_xint), color = "green", linetype = "dashed") +
  geom_vline(data = heat_vline_table, aes(xintercept = vline4_xint), color = "green", linetype = "dashed") +
  geom_vline(data = heat_vline_table, aes(xintercept = vline5_xint), color = "green") +
  labs(x = "Time", y = "seats", title = "Seats available on Stubhub during 2018 First Round") +
  ylim(0, 7500) +
  facet_wrap(~Home_game_num, nrow = 3, labeller = game_labeller) + 
  theme_gray() +
  NULL


celtics_series <- seat_data %>% 
  filter(Season == "2018" & Round == 2 & !(Home_game_num %in% c(3, 4))) %>% 
  group_by(retrieveTime, Round, Home_game_num, date) %>% 
  summarize(sum = sum(quantity), avg_price = mean(ticketPrice))

ggplot(celtics_series, aes(x = retrieveTime, y = sum, color = factor(Home_game_num))) + 
  geom_point() +
  geom_vline(aes(xintercept = ymd_hm("2018-04-30 7:00")), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = ymd_hm("2018-05-03 7:00")), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = ymd_hm("2018-05-05 7:00")), color = "red") +
  geom_vline(aes(xintercept = ymd_hm("2018-05-07 7:00")), color = "green") +
  geom_vline(aes(xintercept = ymd_hm("2018-05-09 7:00")), color = "red", linetype = "dashed") +
  labs(x = "Time", y = "seats", title = "Seats on Stubhub for Celtics Series") +
  ylim(0, 6000) +
  NULL

celtics_vlines <- c(
  ymd_hm("2018-04-30 7:00"),
  ymd_hm("2018-05-03 7:00"),
  ymd_hm("2018-05-05 7:00"),
  ymd_hm("2018-05-07 7:00")
)

celtics_vline_table <- tibble(
  vline1_xint = c(rep(NA, 0), rep(celtics_vlines[1], 2)),
  vline2_xint = c(rep(NA, 0), rep(celtics_vlines[2], 2)),
  vline3_xint = c(rep(NA, 0), rep(celtics_vlines[3], 2)),
  vline4_xint = c(rep(NA , 1), rep(celtics_vlines[4], 1)),
  Home_game_num = c(1, 2)
)

ggplot(celtics_series, aes(x = retrieveTime, y = sum)) + 
  geom_point() +
  geom_vline(data = celtics_vline_table, aes(xintercept = vline1_xint), color = "red", linetype = "dashed") +
  geom_vline(data = celtics_vline_table, aes(xintercept = vline2_xint), color = "red", linetype = "dashed") +
  geom_vline(data = celtics_vline_table, aes(xintercept = vline3_xint), color = "red") +
  geom_vline(data = celtics_vline_table, aes(xintercept = vline4_xint), color = "green") +
  labs(x = "Time", y = "seats", title = "Seats available on Stubhub during 2018 Conference Semifinals") +
  ylim(0, 7500) +
  facet_wrap(~Home_game_num, nrow = 3, labeller = game_labeller) + 
  theme_gray()
NULL


#######

ggplot(filter(seat_data, level != "suite")) +
  geom_histogram(aes(quantity, ..density..), binwidth = 1) +
  geom_vline(aes(xintercept = mean(quantity, na.rm = TRUE)) , color = "red") +
  geom_vline(aes(xintercept = mean(quantity, na.rm = TRUE) + sd(quantity, na.rm = TRUE)), color = "green") +
  geom_vline(aes(xintercept = mean(quantity, na.rm = TRUE) - sd(quantity, na.rm = TRUE)), color = "green") +
  xlim(0, 8) +
  facet_grid(Season ~ level) +
  labs(x = "tickets per listing", title = "Ticket quantity per listing by level, year") +
  theme_light() +
  NULL

first_round_2018 <- seat_data %>% 
  filter(Season == "2018", Round == 1, Home_game_num == 1, daysBeforeGame < 0.4 & daysBeforeGame > 0.39) %>% 
  group_by(daysBeforeGame, Season) %>% 
  summarize(seats = sum(quantity))

first_round_2019 <- seat_data %>% 
  filter(Season == "2019", Round == 1, Home_game_num == 1, daysBeforeGame < 0.52 & daysBeforeGame > 0.51) %>% 
  group_by(daysBeforeGame, Season) %>% 
  summarize(seats = sum(quantity))

first_round <- rbind(first_round_2018, first_round_2019) %>% 
  mutate(moment = paste(Season, " (", round(daysBeforeGame, digits=1), " days before R1 HG1)", sep = ""))

ggplot(first_round, aes(moment, seats)) + 
  geom_bar(stat = "identity") +
  ggtitle("Seats available for R1 HG1 ~0.5 days before the game by Season") +
  theme_light()







### all together

year_plot <- function(year) {
  year_seat_data <- seat_data %>% 
    filter(Season == year & 
             !(Round == 1 & Home_game_num == 4) & 
             !(Season == "2018" & Round == 2 & Home_game_num %in% c(3, 4)) & 
             !(Season == "2019" & Round == 2 & Home_game_num %in% c(4)) & 
             !(Round %in% c(3, 4)))
  
  year_seat_data <- year_seat_data %>% 
    mutate(game = paste("Round ", Round, ", ", case_when(
      Round == 1 ~ game_labeller2(null, Home_game_num),
      Round == 2 ~ game_labeller(null, Home_game_num)
    ), sep = ""))
  
  playoffs <- year_seat_data %>% 
    filter(daysBeforeGame >= -0) %>% 
    group_by(daysBeforeGame, game, Round, Home_game_num, Season) %>% 
    summarize(sum = sum(quantity))
  
  opponents <- ifelse(year == "2018", c("Heat", "Celtics"), c("Nets", "Raptors"))
  
  plot <- plot_ly(data = playoffs, x = ~daysBeforeGame, y = ~sum, color = ~game, legendgroup = ~Season,
                text = ~paste("Round ", Round, '<br>', case_when(
                  Round == 1 ~ paste(game_labeller2(null, Home_game_num), "<br>vs.", opponents[1]),
                  Round == 2 ~ paste(game_labeller(null, Home_game_num), "<br>vs.", opponents[2]),
                  TRUE ~ "?"
                )),
                type = "scatter", mode = "scatter") %>% 
    layout(title = "Ticket supply vs. days before game",
           xaxis = list(range = c(40, 0), autorange = "reversed", title = "Days before game"),
           yaxis = list(title = "Tickets available", range = c(0, 7500)),
           legend = list(tracegroupgap = 250))
  
  return(plot)
}


subplot(year_plot("2018"), year_plot("2019"), shareX = TRUE, nrows = 2) %>% layout(annotations = list(
  list(x = 1.07 , y = 1.025, text = "2018", showarrow = F, xref='paper', yref='paper', font = list(size=15)),
  list(x = 1.07 , y = 0.465, text = "2019", showarrow = F, xref='paper', yref='paper', font = list(size=15)))
)




