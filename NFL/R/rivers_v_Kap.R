library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(gganimate)

seasons <- 2011:2019
data <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  )
})

df <- data %>% 
  filter(!is.na(qb_epa) & !is.na(cpoe)) %>% 
  filter(passer == "C.Kaepernick" | passer == "P.Rivers" | passer == "C.Newton")

rank <- df %>% 
  group_by(passer) %>% 
  summarise(
    EPA = mean(qb_epa),
    CP = mean(cp),
    CPOE = mean(cpoe),
    Ints = sum(interception),
    Snaps = n(),
    Int_per = Ints/Snaps,
    Team = last(posteam)
    
  ) %>% 
  arrange(-EPA)

wp <- df %>% 
  filter(abs(score_differential) <= 9) %>% 
  group_by(passer) %>% 
  summarise(
    EPA = mean(qb_epa),
    CP = mean(cp),
    CPOE = mean(cpoe),
    Ints = sum(interception),
    Snaps = n(),
    Int_per = Ints/Snaps,
    Team = last(posteam)
    
  ) %>% 
  arrange(-EPA)



#wp <- wp %>% unite("id", passer:season, remove = T)
#rank <- rank %>% unite("id", passer:season, remove = T)

# Try one for gganimate-------------------------------------------------------
rank$adjusted <- "No"
wp$adjusted <- "Si"

anim <- rbind(rank,wp)

# Static Plot
p <- ggplot(anim, aes(x = CPOE, y = EPA)) +
  geom_point(aes(colour = Team))
a <- p +
  transition_states(adjusted,
                    transition_length = 2,
                    state_length = 1)
a +
  ease_aes('cubic-in-out') +
  ggtitle('Juego Cerrado: {closest_state}')



