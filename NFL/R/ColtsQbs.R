library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)

#Para no usar notación científica:
options(scipen = 9999)

seasons <- 2012:2019
pbp <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  )
})

qbs <- pbp %>%
  filter(special_teams_play != 1 & penalty != 1 & week <= 17 & posteam =="IND" , !is.na(epa))

qbs$wp <- round(qbs$wp, digits = 2)

luck <- qbs %>%
  filter(passer=)

df <- qbs %>% 
  group_by(wp) %>%
  summarize(
    EPA = mean(qb_epa),
  )
