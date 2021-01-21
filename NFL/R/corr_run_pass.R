library(purrr)
library(dplyr)
library(nflfastR)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(ggdark)
library(here)
library(future)

# Get Data:
plan(multiprocess)
seasons <- 2000:2019
pbp <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  )
})

# Filter data
df <- pbp %>% 
  filter(pass==1 | rush==1 , !is.na(posteam)) %>% 
  select(posteam,season,pass,yards_gained) %>% 
  group_by(posteam,season,pass) %>% 
  summarize(
    yds = sum(yards_gained)/n()
  ) %>% 
  pivot_wider(names_from = pass, values_from = yds) %>% 
  rename(pass_yds = `1`, rush_yds = `0`) %>% 
  mutate(
    posteam = case_when(
      posteam == 'OAK' ~ 'LV',
      posteam == 'SD' ~ 'LAC',
      posteam == 'STL' ~ 'LA',
      TRUE ~ posteam
    )
  )

df <- df %>% filter(posteam != "")
df <- df %>% left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))

# Calculate Correlation
correl <- cor(df$rush_yds,df$pass_yds)


# Plot
fig <- df %>%
    ggplot(aes(x = rush_yds, y = pass_yds)) +
    #vertical line with mean CPOE
    #add points for the QBs with the right colors
    #cex controls pointrl size and alpha the transparency (alpha = 1 is normal)
    geom_point(color = df$team_color, alpha = .6) +
    #add names using ggrepel, which tries to make them not overlap
    #add a smooth line fitting cpoe + epa
    stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm',color = "red")+
    #titles and caption
    labs(x = "Yardas por tierra / Jugada",
         y = "Yardas por pase / Jugada",
         title = "Yardas por jugada",
         subtitle = "Playoffs incluidos | Temporadas: 2000-2019",
         caption = "Datos: @nflfastR | Gráfica: @Landeros_p33"
         ) +
    annotate(geom = "label", x = 3.1 , y = 8.0, size = 4 ,family = "Trebuchet MS",
             color = "#34d5eb",fill="black",label = 'Correlación = 0.2796')+
    #uses the black and white ggplot theme
    theme_bw() +
    #center title with hjust = 0.5
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 10,hjust = 0.5)
    ) +
    #make ticks look nice
    #if this doesn't work, `install.packages('scales')`
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
ggsave('C:/Users/pablo/Desktop/GithubRepos/Experiments/NFL/Plots/rush_pass_corr.png')
  
fig




  