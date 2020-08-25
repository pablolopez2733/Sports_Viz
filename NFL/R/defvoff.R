#------------------------
#% defensive vs offensive
#------------------------


#Load Pakages:
library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)

#Para no usar notación científica:
options(scipen = 9999)

#Load data (Not using nflfastR but pre-cleaned 2019 season data):
data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))


def <- data %>%
  select(defteam,epa,special) %>%
  filter(special==0,!is.na(epa)) %>%
  group_by(defteam) %>%
  summarize(epa = mean(epa))

off <- data %>%
  select(posteam,epa) %>%
  filter(special==0, !is.na(epa)) %>%
  group_by(posteam) %>%
  summarize(epa = mean(epa))

off <- off %>% 
  rename(off_epa = epa, team = posteam)

def <- def %>% 
  rename(def_epa = epa, team = defteam)

df <- merge(off,def, by = "team")


df <- df %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))


#Figure---------------------------------------------------------------
df %>%
  ggplot(aes(x = off_epa , y = def_epa)) +
  #horizontal line with mean EPA
  geom_hline(yintercept = mean(df$def_epa), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept =  mean(df$off_epa), color = "blue", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos
  geom_image(aes(image = team_logo_espn),asp = 16 / 9) +
  #titles and caption
  labs(x = "AVG Offensive EPA",
       y = "AVG Deffensive EPA",
       title = "Offensive and Defensive Efficiency in 2019",
       caption = "Data: @nflfastR") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))


