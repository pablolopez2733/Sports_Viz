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

df <- data %>%
  select(defteam,rush,pass,epa,special) %>%
  filter(special==0,!is.na(epa)) %>%
  mutate(
    rush_epa = case_when(
      rush == 1 ~ epa,
      rush == 0 ~ 0 
    ),
    air_epa = case_when(
      pass == 1 ~ epa,
      pass == 0 ~ 0
    )
  ) %>% 
  group_by(defteam) %>%
  summarize(
    air_epa = mean(air_epa),
    rush_epa = mean(rush_epa)
    )

#--------------------------------FIGURE---------------------------------------
df <- df %>%
  left_join(teams_colors_logos, by = c('defteam' = 'team_abbr'))

df %>%
  ggplot(aes(x = rush_epa , y = air_epa)) +
  #horizontal line with mean EPA
  geom_hline(yintercept = mean(df$air_epa), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept =  mean(df$rush_epa), color = "blue", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos
  geom_image(aes(image = team_logo_espn),asp = 16 / 9) +
  #titles and caption
  labs(x = "EPA/jugada permitido por tierra",
       y = "EPA/jugada permitido por aire",
       title = "Defensa por aire y por tierra ",
       subtitle = "Semanas 1-7 | Temporada 2020-2021",
       caption = "Data: @nflfastR | Gráfica: @Landeros_33") +
  annotate(geom = "label", x = -.07, y = -.06,size=2.5,family = "Trebuchet MS",color = "green2",fill="black",
           label = 'Buena vs Pase | Buena vs corrida')+
  annotate(geom = "label", x = -.07, y = .23,size=2.5,family = "Trebuchet MS",color = "gold2",fill="black",
           label = 'Mala vs Pase | Buena vs corrida')+
  annotate(geom = "label", x = .03, y = -.06,size=2.5,family = "Trebuchet MS",color = "gold2",fill="black",
           label = 'Buena vs Pase | Mala vs corrida')+
  annotate(geom = "label", x = .03, y = .23,size=2.5,family = "Trebuchet MS",color = "red2",fill="black",
           label = 'Mala vs Pase | Mala vs corrida')+
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  scale_y_reverse(breaks = scales::pretty_breaks(n = 10))+
  scale_x_reverse(breaks = scales::pretty_breaks(n = 10))

ggsave('C:/Users/pablo/Desktop/GithubRepos/Experiments/NFL/Plots/airvrush_def.png'
       , height = 6, width = 8)