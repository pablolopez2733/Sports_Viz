#Week 8 EPA per play
library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)

seasons <- 2020
data <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  )
})
