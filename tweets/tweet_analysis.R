library(rtweet); 
library(tidyverse)
library(lubridate)
library(reshape2)
library(modelr)
library("readxl")
library(gsubfn)


setwd("/Users/leojacoby/programming/R/Sixers/tweets")

handles <- c(
  "Bucks",
  "Raptors",
  "sixers",
  "celtics",
  "Pacers",
  "BrooklynNets",
  "OrlandoMagic",
  "DetroitPistons",
  "hornets",
  "MiamiHEAT",
  "WashWizards",
  "ATLHawks",
  "chicagobulls",
  "cavs",
  "nyknicks",
  "warriors",
  "nuggets",
  "trailblazers",
  "HoustonRockets",
  "utahjazz",
  "okcthunder",
  "spurs",
  "LAClippers",
  "SacramentoKings",
  "Lakers",
  "Timberwolves",
  "memgrizz",
  "PelicansNBA",
  "dallasmavs",
  "Suns"
)


tmls <- get_timelines(handles, n = 3200) %>% 
  filter(created_at > ymd_hms("2019-06-20 7:00:00") & created_at < ymd_hms("2019-06-21 7:00:00"))

setwd("/Users/leojacoby/programming/R/Sixers/tweets/data")
save(tmls, file = "tmls.RData")
setwd("/Users/leojacoby/programming/R/Sixers/tweets")

####

load("data/tmls.RData")

# NOTE: can't tell when engagements come in.

tmls <- tmls %>% 
  mutate(media_type = case_when(
    grepl("photo", media_expanded_url) & !grepl("video", media_expanded_url) ~ "photo",
    grepl("video", media_expanded_url) & !grepl("photo", media_expanded_url) ~ "video",
    grepl("photo", media_expanded_url) & grepl("video", media_expanded_url) ~ "combo",
    TRUE ~ "text"
  ))

number_type_summary <- tmls %>% 
  filter(is.na(reply_to_status_id) & !is_retweet) %>% 
  group_by(name, media_type) %>% 
  tally() %>% 
  ungroup() %>% 
  spread(media_type, n) %>% 
  mutate(text = ifelse(is.na(text), 0, text),
         tweets = photo + video + text)

engagement_type_summary <- tmls %>% 
  filter(is.na(reply_to_status_id) & !is_retweet) %>% 
  group_by(name, media_type) %>% 
  summarize(avg_fav = mean(favorite_count, na.rm = T), 
            total_fav = sum(favorite_count, na.rm = T), 
            avg_retweet = mean(retweet_count, na.rm = T),
            total_retweet = sum(retweet_count, na.rm = T))
engagement_type_summary <- melt(engagement_type_summary)
engagement_type_summary <- dcast(engagement_type_summary,name ~ media_type + variable) %>% 
  mutate(text_total_fav = ifelse(is.na(text_total_fav), 0, text_total_fav),
         text_total_retweet = ifelse(is.na(text_total_retweet), 0, text_total_retweet),
         text_avg_fav = ifelse(is.na(text_avg_fav), 0, text_avg_fav),
         text_avg_retweet = ifelse(is.na(text_avg_retweet), 0, text_avg_retweet))

engagement_summary <- tmls %>% 
  filter(is.na(reply_to_status_id) & !is_retweet) %>% 
  group_by(name, followers_count) %>% 
  summarize(all_avg_fav = mean(favorite_count, na.rm = T), 
            all_total_fav = sum(favorite_count, na.rm = T), 
            all_avg_retweet = mean(retweet_count, na.rm = T),
            all_total_retweet = sum(retweet_count, na.rm = T))

tidy_engagement_data <- full_join(engagement_summary, engagement_type_summary, by = c("name"))
tidy_data <- full_join(number_type_summary, tidy_engagement_data, by = c("name")) %>% 
  mutate(wText = tweets / text,
         wPhoto = tweets / photo,
         wVideo = tweets / video,
         all_fol_fav = all_total_fav / followers_count * 1000,
         photo_fol_fav = photo_total_fav / followers_count * 1000 * wPhoto,
         video_fol_fav = video_total_fav / followers_count * 1000 * wVideo,
         text_fol_fav = text_total_fav / followers_count * 1000 * wText,
         all_fol_rt = all_total_retweet / followers_count * 1000,
         photo_fol_rt = photo_total_retweet / followers_count * 1000 * wPhoto,
         video_fol_rt = video_total_retweet / followers_count * 1000 * wVideo,
         text_fol_rt = text_total_retweet / followers_count * 1000 * wText)

setwd("/Users/leojacoby/programming/R/Sixers/tweets/data")
save(tidy_data, file = "tidy_data.RData")
setwd("/Users/leojacoby/programming/R/Sixers/tweets")

###

load("data/tidy_data.RData")
load("data/tmls.RData")

library(plotly)

by_count <- arrange(tidy_data, desc(tweets))

tmls$name <- factor(tmls$name, levels = by_count$name)

ggplot(filter(tmls, is.na(reply_to_status_id) & !is_retweet), aes(x = name)) +
  geom_bar(aes(fill = media_type)) +
  labs(x = "Team", y = "Number of tweets", title = "Number of tweets by team (non-replies only)") +
  scale_fill_discrete(name = "Tweet type", labels = c("photo", "just text", "video")) +
  theme_light() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

(fav_fol_all <- plot_ly(tidy_data, x = ~reorder(name, -all_fol_fav), y = ~all_fol_fav, type = 'bar', name = "Favorites") %>% 
  layout(title = 'Favorites per 1k followers (original tweets and quoted retweets only)', xaxis = list(title = ''), yaxis = list(title = 'Favorites per 1k followers')))

(rt_fol_all <- plot_ly(tidy_data, x = ~reorder(name, -all_fol_fav), y = ~all_fol_rt, type = 'bar', name = "Retweets") %>% 
  layout(title = 'Retweets per 1k followers (original tweets and quoted retweets only)', xaxis = list(title = ''), yaxis = list(title = 'Retweets per 1k followers')))

(big_plot <- subplot(fav_fol_all, rt_fol_all, shareX = T, nrows = 2) %>% 
    layout(title = "Engagement per 1k followers (original tweets and quoted retweets only)"))



(fav_avg_photo <- plot_ly(tidy_data, x = ~reorder(name, -photo_avg_fav), y = ~photo_avg_fav, type = 'bar', name = "Favorites") %>% 
    layout(title = 'Favorites per tweet (original tweets and quoted retweets only)', xaxis = list(title = ''), yaxis = list(title = 'Favorites per photo tweet')))

(rt_avg_photo <- plot_ly(tidy_data, x = ~reorder(name, -photo_avg_fav), y = ~photo_avg_retweet, type = 'bar', name = "Retweets") %>% 
    layout(title = 'Retweets per tweet (original tweets and quoted retweets only)', xaxis = list(title = ''), yaxis = list(title = 'Retweets per photo tweet')))

(big_plot_photo <- subplot(fav_avg_photo, rt_avg_photo, shareX = T, nrows = 2) %>% 
    layout(title = "Engagement per photo tweet (original tweets and quoted retweets only)"))


(fav_avg_video <- plot_ly(tidy_data, x = ~reorder(name, -video_avg_fav), y = ~video_avg_fav, type = 'bar', name = "Favorites") %>% 
    layout(title = 'Favorites per video tweet (original tweets and quoted retweets only)', xaxis = list(title = ''), yaxis = list(title = 'Favorites per video tweet', range = c(0, 5000))))

(rt_avg_video <- plot_ly(tidy_data, x = ~reorder(name, -video_avg_fav), y = ~video_avg_retweet, type = 'bar', name = "Retweets") %>% 
    layout(title = 'Retweets per video tweet (original tweets and quoted retweets only)', xaxis = list(title = ''), yaxis = list(title = 'Retweets per video tweet', range = c(0, 1700))))

(big_plot_video <- subplot(fav_avg_video, rt_avg_video, shareX = T, nrows = 2) %>% 
    layout(title = "Engagement per video tweet (original tweets and quoted retweets only)"))



(fav_avg_text <- plot_ly(tidy_data, x = ~reorder(name, -text_avg_fav), y = ~text_avg_fav, type = 'bar', name = "Favorites") %>% 
    layout(title = 'Favorites per text tweet (original tweets and quoted retweets only)', xaxis = list(title = ''), yaxis = list(title = 'Favorites per text tweet')))

(rt_avg_text <- plot_ly(tidy_data, x = ~reorder(name, -text_avg_fav), y = ~text_avg_retweet, type = 'bar', name = "Retweets") %>% 
    layout(title = 'Retweets per text tweet (original tweets and quoted retweets only)', xaxis = list(title = ''), yaxis = list(title = 'Retweets per text tweet')))

(big_plot_text <- subplot(fav_avg_text, rt_avg_text, shareX = T, nrows = 2) %>% 
    layout(title = "Engagement per text tweet (original tweets and quoted retweets only)"))

test_tidy <- tidy_data %>% 
  mutate(one_over = (1/tweets))
              
model <- lm(all_total_fav ~ tweets, data = test_tidy)


ggplot(tidy_data, aes(x = tweets, y = all_total_fav)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "Number of tweets vs. Average favorites per tweet", y = "favorites per tweet") +
  theme_light()

(followers_bar <- plot_ly(tidy_data, x = ~reorder(name, -followers_count), y = ~followers_count, type = 'bar') %>% 
    layout(title = 'Followers for team official accounts', xaxis = list(title = ''), yaxis = list(title = 'followers')))


#### followers controlled

(fav_fol_all <- plot_ly(tidy_data, x = ~reorder(name, -all_fol_fav), y = ~all_fol_fav, type = 'bar', name = "Favorites") %>% 
    layout(title = 'Favorites per 1k followers (original tweets and quoted retweets only)', xaxis = list(title = ''), yaxis = list(title = 'Favorites per 1k followers', range = c(0, 40))))

(rt_fol_all <- plot_ly(tidy_data, x = ~reorder(name, -all_fol_fav), y = ~all_fol_rt, type = 'bar', name = "Retweets") %>% 
    layout(title = 'Retweets per 1k followers (original tweets and quoted retweets only)', xaxis = list(title = ''), yaxis = list(title = 'Retweets per 1k followers', range = c(0, 10))))

(big_plot_fol_all <- subplot(fav_fol_all, rt_fol_all, shareX = T, nrows = 2, margin = 0.07) %>% 
    layout(title = "Engagement per 1k followers (original tweets and quoted retweets only)"))


(fav_fol_photo <- plot_ly(tidy_data, x = ~reorder(name, -photo_fol_fav), y = ~photo_fol_fav, type = 'bar', name = "Favorites") %>% 
    layout(title = 'Favorites per 1k followers (original tweets and quoted retweets only)', xaxis = list(title = ''), yaxis = list(title = 'Favorites per 1k followers', range = c(0, 60))))

(rt_fol_photo <- plot_ly(tidy_data, x = ~reorder(name, -photo_fol_fav), y = ~photo_fol_rt, type = 'bar', name = "Retweets") %>% 
    layout(title = 'Retweets per 1k followers (original tweets and quoted retweets only)', xaxis = list(title = ''), yaxis = list(title = 'Retweets per 1k followers', range = c(0, 15))))

(big_plot_fol_photo <- subplot(fav_fol_photo, rt_fol_photo, shareX = T, nrows = 2, margin = 0.07) %>% 
    layout(title = "Photo engagement per 1k followers (original tweets and quoted retweets only)"))


(fav_fol_video <- plot_ly(tidy_data, x = ~reorder(name, -video_fol_fav), y = ~video_fol_fav, type = 'bar', name = "Favorites") %>% 
    layout(title = 'Favorites per 1k followers (original tweets and quoted retweets only)', xaxis = list(title = ''), yaxis = list(title = 'Favorites per 1k followers', range = c(0, 60))))

(rt_fol_video <- plot_ly(tidy_data, x = ~reorder(name, -video_fol_fav), y = ~video_fol_rt, type = 'bar', name = "Retweets") %>% 
    layout(title = 'Retweets per 1k followers (original tweets and quoted retweets only)', xaxis = list(title = ''), yaxis = list(title = 'Retweets per 1k followers', range = c(0, 15))))

(big_plot_fol_video <- subplot(fav_fol_video, rt_fol_video, shareX = T, nrows = 2, margin = 0.07) %>% 
    layout(title = "Video engagement per 1k followers (original tweets and quoted retweets only)"))



(fav_fol_text <- plot_ly(tidy_data, x = ~reorder(name, -text_fol_fav), y = ~text_fol_fav, type = 'bar', name = "Favorites") %>% 
    layout(title = 'Favorites per 1k followers (original tweets and quoted retweets only)', xaxis = list(title = ''), yaxis = list(title = 'Favorites per 1k followers')))

(rt_fol_text <- plot_ly(tidy_data, x = ~reorder(name, -text_fol_fav), y = ~text_fol_rt, type = 'bar', name = "Retweets") %>% 
    layout(title = 'Retweets per 1k followers (original tweets and quoted retweets only)', xaxis = list(title = ''), yaxis = list(title = 'Retweets per 1k followers')))

(big_plot_fol_text <- subplot(fav_fol_text, rt_fol_text, shareX = T, nrows = 2) %>% 
    layout(title = "Text engagement per 1k followers (original tweets and quoted retweets only)"))


## picks analysis

pick <- 1:60

pick_values <- tibble(pick) %>% 
  mutate(value = 100 - 23 * log(pick, base = exp(1)))

name <- c(
  "New Orleans Pelicans",
  "Memphis Grizzlies",
  "NEW YORK KNICKS",
  "Atlanta Hawks",
  "Cleveland Cavaliers",
  "Timberwolves",
  "Chicago Bulls",
  "New Orleans Pelicans",
  "Washington Wizards",
  "Atlanta Hawks",
  "Phoenix Suns",
  "Charlotte Hornets",
  "Miami HEAT",
  "Boston Celtics",
  "Detroit Pistons",
  "Orlando Magic",
  "New Orleans Pelicans",
  "Indiana Pacers",
  "San Antonio Spurs",
  "Philadelphia 76ers",
  "Memphis Grizzlies",
  "Boston Celtics",
  "OKC THUNDER",
  "Phoenix Suns",
  "Portland Trail Blazers",
  "Cleveland Cavaliers",
  "LA Clippers",
  "Golden State Warriors",
  "San Antonio Spurs",
  "Cleveland Cavaliers",
  "Brooklyn Nets",
  "Miami HEAT",
  "Boston Celtics",
  "Atlanta Hawks",
  "New Orleans Pelicans",
  "Charlotte Hornets",
  "Detroit Pistons",
  "Chicago Bulls",
  "Golden State Warriors",
  "Sacramento Kings",
  "Golden State Warriors",
  "Washington Wizards",
  "Timberwolves",
  "Denver Nuggets",
  "Dallas Mavericks",
  "Los Angeles Lakers",
  "NEW YORK KNICKS",
  "LA Clippers",
  "San Antonio Spurs",
  "Utah Jazz",
  "Boston Celtics",
  "Charlotte Hornets",
  "Utah Jazz",
  "Philadelphia 76ers",
  "Sacramento Kings",
  "Brooklyn Nets",
  "Detroit Pistons",
  "Utah Jazz",
  "Toronto Raptors",
  "Sacramento Kings"
)

international <- c(
  "USA",
  "USA",
  "Canada",
  "USA",
  "USA",
  "USA",
  "USA",
  "USA",
  "International",
  "USA",
  "USA",
  "USA",
  "USA",
  "USA",
  "International",
  "USA", #chuma
  "Canada", #nickeil
  "International", #goga
  "International", #luka
  "USA",
  "Canada", #brandon
  "USA", 
  "USA", 
  "USA", 
  "USA", 
  "USA",
  "Canada", #kabengele
  "USA",
  "USA",
  "USA",
  rep(NA, 30)
)

# assuming that international effect of second rounders is negligible

draft <- tibble(points = pick_values$value, name, international) %>% 
  group_by(name) %>% 
  summarize(points = sum(points), canada = any(international == "Canada", na.rm = T), international = any(international == "International", na.rm = T))

rockets_vector <- c("Houston Rockets", 0, FALSE, FALSE)
bucks_vector <- c("Milwaukee Bucks", 0, F, F)

draft <- rbind(draft, rockets_vector, bucks_vector) %>% mutate(points = as.numeric(points))

tight_tidy_data <- tidy_data %>%  dplyr::select(name, tweets,
                                         wText, wPhoto, wVideo, 
                                         all_fol_fav, all_fol_rt, 
                                         video_fol_fav, video_fol_rt,
                                         photo_fol_fav, photo_fol_rt,
                                         text_fol_fav, text_fol_rt) %>% 
  mutate(engage_fol_all = all_fol_fav + all_fol_rt)

draft_model_data_all <- full_join(draft, tight_tidy_data, by = c("name"))

win_pct <- c(
  0.354,
  0.598,
  0.512,
  0.476,
  0.268,
  0.232,
  0.402,
  0.659,
  0.5,
  0.695,
  0.58,
  0.585,
  0.451,
  0.402,
  0.476,
  0.402,
  0.207,
  0.598,
  0.512,
  0.622,
  0.232,
  0.646,
  0.476,
  0.585,
  0.439,
  0.707,
  0.61,
  0.39,
  0.646,
  0.732
  )

playoff_success <- c(
0,
2,
1,
0,
0,
0,
0,
2,
1,
4,
1,
1,
0,
0,
0,
0,
0,
1,
1,
2,
0,
3,
0,
1,
0,
5,
1,
0,
2,
3
)

draft_model_data_all$win_pct <- win_pct
draft_model_data_all$playoff_success <- playoff_success
draft_model_data_all$season <- rep("2019", 30)

# write_csv(draft_model_data_all, path = "draft_model_data.csv")

# setwd("/Users/leojacoby/programming/R/Sixers/tweets/data")
# draft_model_data_all <- read_csv("draft_model_data_enhanced.csv")
# setwd("/Users/leojacoby/programming/R/Sixers/tweets")

draft_model_data <-  draft_model_data_all %>% 
  filter(!(name %in% c("Washington Wizards")))

model <- lm(engage_fol_all ~ points + win_pct + playoff_success + canada + international + wPhoto + wVideo + tweets, data = draft_model_data)
regression_line <- lm(engage_fol_all ~ points, draft_model_data)
regression_line2 <- lm(engage_fol_all ~ win_pct, draft_model_data)


(points_plot <- plot_ly(data = draft_model_data, x = ~points, y = ~engage_fol_all, 
                        type = "scatter", mode = "markers")   %>% 
    add_lines(x = ~points, y = ~fitted(regression_line)) %>% 
    layout(title = 'Effect of draft pick quality and quantity on twitter engagement', 
           xaxis = list(title = "Draft points"),
           yaxis = list(title = "engagements per 1k followers"),
           showlegend = F,
           margin = 0))

(win_pct_plot <- plot_ly(data = draft_model_data, x = ~win_pct, y = ~engage_fol_all, 
                        type = "scatter", mode = "markers")   %>% 
    add_lines(x = ~win_pct, y = ~fitted(regression_line2)) %>% 
    layout(title = 'Effect of season winning percentage on twitter engagement', 
           xaxis = list(title = "Win pct"),
           yaxis = list(title = "engagements per 1k followers"),
           showlegend = F,
           margin = 0))

perscriptive_model <- lm(engage_fol_all ~ points + win_pct  + canada, data = draft_model_data)

draft_model_data_all_pred_resid <- draft_model_data_all %>% 
  add_predictions(perscriptive_model) %>% 
  add_residuals(perscriptive_model)


####### Set up old data #########

setwd("/Users/leojacoby/programming/R/Sixers/tweets/data")
tmls_old <- read_excel("tmls_old.xlsx") %>% dplyr::select(-replies)
tmls_2019 <- read_csv("tmls_2019.csv") #%>% mutate(timestamp = as.POSIXct(timestamp, origin="1970-01-01"))
setwd("/Users/leojacoby/programming/R/Sixers/tweets")

tmls_old$date = sapply(tmls_old$timestamp,function(x){ 
  strsplit(x,'T')[[1]][1]
  })

tmls_old$time = sapply(tmls_old$timestamp,function(x){ 
  strsplit(strsplit(x,'T')[[1]][2], '\\+')[[1]][1]
})

tmls_old <- tmls_old %>% 
  mutate(timestamp = ymd_hms(paste(date, time)),
         season = year(timestamp),
         type) %>% 
  dplyr::select(-date, -time) %>% 
  filter(case_when(
    season == 2018 ~ (timestamp > ymd_hms("2018-06-21 7:00:00") & timestamp < ymd_hms("2018-06-22 7:00:00")),
    season == 2017 ~ (timestamp > ymd_hms("2017-06-22 7:00:00") & timestamp < ymd_hms("2017-06-23 7:00:00")),
    TRUE ~ TRUE
  ))

tmls_2019 <- tmls_2019 %>% 
  mutate(timestamp = as.POSIXct(timestamp, origin = "1970-01-01"),
         season = year(timestamp)) %>% 
  dplyr::select(-time) %>% 
  filter(timestamp > ymd_hms("2019-06-20 7:00:00") & timestamp < ymd_hms("2019-06-21 7:00:00"))

tmls_all <- rbind(tmls_2019, tmls_old) %>% 
  mutate(type = case_when(
    grepl("video", tolower(media_url)) ~ "video",
    grepl("photo", tolower(type)) ~ "photo",
    grepl("link", tolower(type)) ~ "link",
    grepl("text only", tolower(type)) ~ "text"
  ))

#######  get tidy data for tmls old ########

number_type_summary_all <- tmls_all %>% 
  group_by(brand, season, type) %>% 
  tally() %>% 
  ungroup() %>% 
  spread(type, n) %>% 
  mutate(text = ifelse(is.na(text), 0, text),
         link = ifelse(is.na(link), 0, link),
         video = ifelse(is.na(video), 0, video),
         photo = ifelse(is.na(photo), 0, photo),
         tweets = photo + video + text + link)

engagement_type_summary_all <- tmls_all %>% 
  group_by(brand, season, type) %>% 
  summarize(avg_engage = mean(total_public_interactions, na.rm = T), 
            total_engage = sum(total_public_interactions, na.rm = T))
engagement_type_summary_all <- melt(engagement_type_summary_all, id.vars = c("brand", "type", "season"))
engagement_type_summary_all <- dcast(engagement_type_summary_all,brand + season ~ type + variable)

engagement_type_summary_all <- engagement_type_summary_all %>% 
  mutate(text_avg_engage = ifelse(is.na(text_avg_engage), 0, text_avg_engage),
         text_total_engage = ifelse(is.na(text_total_engage), 0, text_total_engage),
         link_avg_engage = ifelse(is.na(link_avg_engage), 0, link_avg_engage),
         link_total_engage = ifelse(is.na(link_total_engage), 0, link_total_engage),
         photo_avg_engage = ifelse(is.na(photo_avg_engage), 0, photo_avg_engage),
         photo_total_engage = ifelse(is.na(photo_total_engage), 0, photo_total_engage),
         video_avg_engage = ifelse(is.na(video_avg_engage), 0, video_avg_engage),
         video_total_engage = ifelse(is.na(video_total_engage), 0, video_total_engage))

engagement_summary_all <- tmls_all %>% 
  group_by(brand, season) %>% 
  summarize(all_avg_engage = mean(total_public_interactions, na.rm = T), 
            all_total_engage = sum(total_public_interactions, na.rm = T))

followers_tbl <- tidy_data %>% 
  dplyr::select(name, followers_count) %>% 
  mutate(name = case_when(
    name == "LA Clippers" ~ "Los Angeles Clippers",
    name == "Miami HEAT" ~ "Miami Heat",
    name == "NEW YORK KNICKS" ~ "New York Knicks",
    name == "OKC THUNDER" ~ "Oklahoma City Thunder",
    name == "Timberwolves" ~ "Minnesota Timberwolves",
    TRUE ~ name
  ))

followers_tbl_2017 <- followers_tbl %>% mutate(season = 2017)
followers_tbl_2018 <- followers_tbl %>% mutate(season = 2018)
followers_tbl_2019 <- followers_tbl %>% mutate(season = 2019)

followers_tbl_all <- rbind(followers_tbl_2017, followers_tbl_2018, followers_tbl_2019) %>% dplyr::rename(brand = name)




tidy_engagement_data_all <- full_join(engagement_summary_all, engagement_type_summary_all, by = c("brand", "season"))
tidy_data_all <- full_join(number_type_summary_all, tidy_engagement_data_all, by = c("brand", "season")) 
tidy_data_all <- full_join(tidy_data_all, followers_tbl_all, by = c("brand", "season"))

tidy_data_all <- tidy_data_all %>% 
  mutate(wText = tweets / text,
         wLink = tweets / link,
         wPhoto = tweets / photo,
         wVideo = tweets / video,
         all_fol_engage = all_total_engage / followers_count * 1000)






