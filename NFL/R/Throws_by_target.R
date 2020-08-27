library(purrr)
library(dplyr)
library(nflfastR)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(ggdark)
library(here)

seasons <- 2019
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/legacy-data/play_by_play_{x}.rds")
    )
  )
})


id_pos<-readRDS(
  url(("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/roster-data/roster.rds"))) %>% 
  dplyr::select(
    receiver_position=teamPlayers.position, 
    receiver_id = teamPlayers.gsisId) %>% distinct %>% filter(!is.na(receiver_id),!duplicated(receiver_id))


pbp_merged<-dplyr::left_join(pbp,id_pos,by='receiver_id',na_matches='never')

qbs <- pbp_merged %>%
  filter(!is.na(receiver_position)) %>%
  group_by(passer) %>%
  summarize(
    TE = sum(receiver_position == "TE"),
    RB = sum(receiver_position == "RB" | receiver_position == "FB"),
    WR = sum(receiver_position == "WR"),
    n_dropbacks = sum(pass),
    team = last(posteam)
  ) %>%
  ungroup() %>%
  filter(n_dropbacks > 100)

qbs <- qbs %>%
  mutate(TE_per = TE/n_dropbacks,
         RB_per = RB/n_dropbacks,
         WR_per = WR/n_dropbacks
           )

qbs <- qbs %>%
  filter(n_dropbacks > 200)%>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))


#FIGURE-------------------------------------------------------------------

ffplot <- qbs %>%
  ggplot(aes(x = RB_per*100, y = TE_per*100)) +
  geom_hline(yintercept = mean(qbs$TE_per)*100, color = "#f75ce5", linetype = "dashed", alpha=0.9) +
  geom_vline(xintercept =  mean(qbs$RB_per)*100, color = "#f75ce5", linetype = "dashed", alpha=0.9) +
  xlim(8, 35)+
  #ylim(0,2.5)+
  #geom_point(color = qbs$team_color3, cex=qbs$n_dropbacks / 75, alpha = .8) +
  geom_image(aes(image = team_logo_espn), asp = 16 / 9)+
  geom_text_repel(aes(label=passer),colour = "white",
                  box.padding = unit(0.5, "lines"),
                  alpha = 0.9,
                  size = 3.5,) +
  labs(x = "% de pases dirigidos a RBs/FBs",
       y = "% de pases dirigidos a TEs",
       title = "% de pases lanzados por posición del receptor",
       subtitle = "Solo 3 QB lanzan más a sus RBs y TEs que a sus WRs: Carson Wentz, Lamar Jackson, Kirk Cousins",
       caption = "Temporada 2019-2020 | Consideramos QBs con más de 200 pases |
       | Gráfica por Pablo L. Landeros |
       | Datos: @nflfastR") +
  dark_theme_gray() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 16, family = "Trebuchet MS",color = "white",hjust = 0),
    plot.subtitle = element_text(size = 10, family = "Trebuchet MS",color = "white",hjust = 0),
    axis.title = element_text(size = 9, family = "Trebuchet MS",color = "#adadad"),
    plot.caption = element_text(size=8, family = "Trebuchet MS",color = "#adadad",hjust = 0),
    #legend.text = element_text(size = 8, family = "Calibri",color = "#adadad"),
    #legend.direction = "horizontal",
    #legend.position = "bottom",
    #legend.key.size =unit(.75,"line"),
    axis.text = element_text(size = 9, family = "Trebuchet MS",color = "#adadad"),
    panel.grid = element_line(size=.11))+
  scale_y_continuous(labels=function(x) paste0(x,"%"),breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(labels=function(x) paste0(x,"%"),breaks = scales::pretty_breaks(n = 10))


ffplot
ggsave(filename = here::here("/Desktop/GithubRepos/Experiments/NFL/Plots/ff_logo.png"), 
       height = 6, width = 8)



#is there anyone who passes more to TE or rbs than wr?
rare <- qbs[qbs$TE+qbs$RB > qbs$WR, ]





