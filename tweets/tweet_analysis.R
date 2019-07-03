library(rtweet) 
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
         text_total_retweet = ifelse(is.na(text_total_retweet), 0, text_total_retweet))

engagement_summary <- tmls %>% 
  filter(is.na(reply_to_status_id) & !is_retweet) %>% 
  group_by(name) %>% 
  summarize(all_avg_fav = mean(favorite_count, na.rm = T), 
            all_total_fav = sum(favorite_count, na.rm = T), 
            all_avg_retweet = mean(retweet_count, na.rm = T),
            all_total_retweet = sum(retweet_count, na.rm = T))

tidy_engagement_data <- full_join(engagement_summary, engagement_type_summary, by = "name")
tidy_data <- full_join(number_type_summary, tidy_engagement_data, by = "name")



ggplot(reorder(tmls, n), aes(x = name)) +
  geom_bar(aes(fill = media_type)) +
  labs(x = "Team", y = "Number of tweets", title = "Number of tweets by team (non-replies only)") +
  theme_light() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


m2 = reshape(m, idvar=c("name",'variable'), timevar=c('media_type'), direction="wide")
reshape(m2, idvar=c("name"), timevar=c('variable'), direction="wide")          
