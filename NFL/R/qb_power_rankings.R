#Power rankings Ofensivas EPA
library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(dplyr)
library(gganimate)


seasons <- 2020
data <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  )
})

# Filter data
off <- data %>% 
  filter(!is.na(epa) & !is.na(posteam) & special==0 & week <= 9) %>%
  group_by(posteam, week) %>%
  summarize(
    epa = mean(epa)
  ) %>%
  ungroup()

off <- off %>%
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))

# Plot
p <- ggplot(off,aes(week, epa, group = posteam)) +
  geom_line() +
  labs(x = "Semana", 
       y = "EPA",
       title = "Ranking de ofensivas por semana ",
       subtitle = "Semanas 1-9 | Temporada 2020-2021",
       caption = "Data: @nflfastR | Gráfica: @Landeros_33") +
  theme_bw() +
  #center title
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0.5, face = "bold"),
    legend.position = "none"
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
  
p + 
  geom_image(aes(image = team_logo_espn),asp = 16 / 9) +
  #geom_point()+
  transition_reveal(week)

anim_save('C:/Users/pablo/Desktop/GithubRepos/Experiments/NFL/Plots/analytics_pw_off.gif'
       , height = 6, width = 8)

#Data viz guide----------------------------------------------------------------
p_ranking <- off %>% 
  ggplot(aes(x = week, y = epa, group = posteam)) +
  geom_segment(aes(xend = 11, yend = epa), linetype = 2, color = 'grey') +
  geom_line(aes(color = team_color), alpha = 0.7, size = 2) +
  geom_image(aes(image = team_logo_wikipedia), size = 0.035, by = "width", asp = 16/9) +
  scale_color_identity(aesthetics = c("color", "fill")) +
  ggthemes::theme_fivethirtyeight() +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    aspect.ratio = 16/9,
    axis.title.x = element_text(face = "bold")
  ) +
  scale_x_continuous(breaks = seq(0, 14, 1)) +
  coord_cartesian(clip = "off") + 
  transition_reveal(week) + 
  labs(title = 'Ranking de ofensivas por semana',
       subtitle = "Semanas 1-9",
       x = "Semana",
       y = "EPA",
       caption = "Data: ESPN | Plot: @Landeros_p33")

# Save Plot
animate(p_ranking, height = 1000, width = 1000, fps = 20, duration = 20, end_pause = 5)
anim_save('C:/Users/pablo/Desktop/GithubRepos/Experiments/NFL/Plots/analytics_pw_off.gif')