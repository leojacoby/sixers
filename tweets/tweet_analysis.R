library(rtweet); 
library(tidyverse)
library(lubridate)
library(reshape2)

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
  mutate(all_fol_fav = all_total_fav / followers_count * 1000,
         photo_fol_fav = photo_total_fav / followers_count * 1000,
         video_fol_fav = video_total_fav / followers_count * 1000,
         text_fol_fav = text_total_fav / followers_count * 1000,
         all_fol_rt = all_total_fav / followers_count * 1000,
         photo_fol_rt = photo_total_fav / followers_count * 1000,
         video_fol_rt = video_total_fav / followers_count * 1000,
         text_fol_rt= text_total_fav / followers_count * 1000)

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
              
model <- lm(all_avg_fav ~ tweets, data = test_tidy)


ggplot(tidy_data, aes(x = tweets, y = all_avg_fav)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "Number of tweets vs. Average favorites per tweet", y = "favorites per tweet") +
  theme_light()


