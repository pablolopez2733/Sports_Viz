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
  labs(x = "AVG Rush EPA per play allowed",
       y = "AVG Air EPA per play allowed",
       title = "Passing and Rushing Defense through 2020",
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

