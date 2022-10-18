# Is the nfl more boring than before?
library(tidyverse)
library(nflfastR)
library(gt)
library(RColorBrewer)
library(extrafont)
library(ggdark)

font_import()
loadfonts(device = "win")

future::plan("multisession")
seasons <- 1999:2021
pbp <- nflfastR::load_pbp(seasons)

sort(names(pbp))

garb_df <- pbp %>% 
  filter(season != 2021) %>% 
  filter(qtr <= 4 & !is.na(vegas_wp) & season_type == 'REG') %>% 
  group_by(game_id) %>% 
  fill(game_seconds_remaining) %>% 
  mutate(
    sec_elap = -diff(c(game_seconds_remaining,0)),
    fav_wp = ifelse(vegas_wp < .5, 1 - vegas_wp, vegas_wp),
    above_thresh = ifelse(fav_wp > 0.9, 'Above Thresh', 'Below Thresh')
  ) %>% 
  group_by(season, above_thresh) %>% 
  summarise(mins = sum(sec_elap, na.rm = T) / 60) %>% 
  group_by(season) %>%
  mutate(tot_week = sum(mins)) %>% 
  ungroup %>% 
  mutate(pct_thresh = mins / tot_week) %>% 
  filter(above_thresh == 'Below Thresh') %>% 
  arrange(season)

big_play_df <- pbp %>% 
  filter(season != 2021) %>% 
  filter(season_type == 'REG') %>% 
  group_by(season) %>% 
  summarise(
    big_plays = sum(ifelse(abs(epa) > 2, 1, 0), na.rm = T),
    big_play_rt = mean(ifelse(abs(epa) > 2, 1, 0), na.rm = T),
    .groups = 'drop'
  ) %>% 
  arrange(season)


p_df <- big_play_df %>% 
  left_join(garb_df)

# Plot ===================================================================
p_df %>% 
  ggplot(aes(x = season, y = mins)) +
  geom_point(aes(colour = big_plays), size = 5)+
  geom_smooth(colour="white",alpha=0.05,se = T, fill = "white")+
  dark_theme_classic()+
  theme(text= element_text(family="Times", color = "white"),
        aspect.ratio = 9 / 16,
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(angle = 0, vjust = 0.5, face = "bold"),
        legend.title.align=0.5,
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        #axis.line = element_line(colour = "#3b30ff"),
        plot.caption = element_text(hjust = 0)
        )+
  #make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5),
                     labels = scales::comma) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 22))+
  scale_color_viridis_c(name = "Big plays\nper season")+
  # scale_color_gradient(name = "Big plays\nper season",
  #                      low = "yellow",
  #                      high = "red")+
  # scale_color_gradient(low = "white", high = "red",
  #                      labels = c("Few big plays ",
  #                                 "Lots of big plays"),
  #                      breaks = c(min(p_df$big_plays)+10,
  #                                 max(p_df$big_plays)-100))+
  labs(title = "Football was pretty boring this week,\nbut it is (slightly) more fun than 20 years ago", 
       subtitle = "Big Plays and Competitiveness throughout last 21 seasons",
       y = 'Regulation\nMins w/\nSpread-Adj\nWin Prob\nBetween\n10% & 90%',
       x = 'Season',
       caption = "Big Plays : Less than -2 EPA or More than +2 EPA \nFigure: Landeros_p33 | Original code: @reinhurdler \nData: @nflfastR"
       )
ggsave(filename = paste0(getwd(),"/plots/boring_nfl_plot2.png"),
       height = 8, width = 10, dpi = 600)
 
