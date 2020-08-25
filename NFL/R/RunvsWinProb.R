library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(data.table)
library(formattable)
library(tidyr)
library(DT)
library(ggrepel)
library(ggdark)
library(ggimage)
library(nflfastR)

#Read Data
data19 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))
seasons <- 2015:2019
data_1519 <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  )
})

rps <- data_1519 %>%
  filter(rush == 1 & penalty != 1 & week <= 17  , !is.na(epa))

rps$wp <- round(rps$wp, digits = 2)

df <- rps %>% 
  group_by(wp) %>%
  summarize(
    EPA = mean(epa),
  )

df <- df %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))


ggplot(df,aes(x=wp,y=EPA))+
  geom_point() +
  geom_smooth()+
  labs(x = "Rush EPA",
       y = "Win Probability",
       title = "Rush EPA vs Win Probability (2015-2019)",
       caption = "Data: @nflfastR") +
  dark_theme_gray()

