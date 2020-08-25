library(purrr)
library(dplyr)
library(nflfastR)
library(ggrepel)
library(ggimage)
library(nflfastR)

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



#FIGURE-------------------------------------------------------------------
qbs <- qbs %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

qbs %>%
  ggplot(aes(x = RB_per, y = TE_per)) +
  #horizontal line with mean EPA
  geom_hline(yintercept = mean(qbs$TE_per), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept =  mean(qbs$RB_per), color = "blue", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the right colors
  #cex controls point size and alpha the transparency (alpha = 1 is normal)
  geom_point(color = qbs$team_color, cex=qbs$n_dropbacks / 75, alpha = .6) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=passer)) +
  #titles and caption
  labs(x = "% de pases dirigidos a RBs/FBs",
       y = "% de pases dirigidos a TEs",
       title = "% de pases lanzados por posición del receptor (Temporada 2019-2020)",
       subtitle = "El tamaño de las burbujas depende de la cantidad de pases lanzados",
       caption = "Data: @nflfastR") +
  #uses the black and white ggplot theme
  theme_bw() +
  #center title with hjust = 0.5
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  #if this doesn't work, `install.packages('scales')`
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))





#is there anyone who passes more to TE or rbs than wr?
rare <- qbs[qbs$TE+qbs$RB > qbs$WR, ]





