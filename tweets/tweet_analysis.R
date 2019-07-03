library(rtweet) 
library(tidyverse)
library(lubridate)

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
