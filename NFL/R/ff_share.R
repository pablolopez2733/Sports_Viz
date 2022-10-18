library(tidyverse)
library(nflfastR)
library(ggrepel)
library(gt)
library(ggimage)
library(hrbrthemes)
library(extrafont)
loadfonts(device = "win")

future::plan("multisession")

seasons <- 2021
pbp <- nflfastR::load_pbp(seasons)
roster <- nflfastR::fast_scraper_roster(seasons)

# join para las posiciones de TEs y WR
joined_wr <- pbp %>% 
  dplyr::filter(!is.na(receiver_id) & pass == 1  & !is.na(air_yards)) %>%
  dplyr::left_join(roster, by = c("receiver_id" = "gsis_id"))

total_passes <- pbp %>% 
  filter(pass == 1) %>% 
  group_by(posteam) %>% 
  summarise(passes = n())

wr <- joined_wr %>% 
  filter(position %in% c("WR")) %>% 
  dplyr::group_by(posteam,receiver_id, receiver) %>%
  dplyr::summarize( targets = n(),
                    air_yards = sum(air_yards),
                    yards_gained = sum(yards_gained)) %>%
  left_join(total_passes) %>% 
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr')) %>% 
  mutate(ts = targets / passes,
         adot = air_yards / targets) %>% 
  dplyr::arrange(-targets) %>% 
  head(30)

# grafica wrs =====================
wr %>%
  ggplot(aes(x = ts, y = adot)) +
  #horizontal line with mean EPA
  geom_hline(yintercept = mean(wr$adot), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept =  mean(wr$ts), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the right colors
  #cex controls point size and alpha the transparency (alpha = 1 is normal)
  #geom_point(color = wr$team_color) +
  #geom_image(aes(image = team_logo_espn), asp = 16 / 9) +
  geom_image(aes(image = team_logo_espn),
             size = 0.04,
             ##by="height",
             asp = 16 / 9 )+
  scale_size_identity()+
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=receiver),
                  family="Trebuchet MS",
                  colour = "#e8e8e8",
                  force_pull = 0.3,
                  box.padding = 0.6,
                  verbose = TRUE,
                  seed = 123,
                  max.time = 1,
                  max.iter = Inf,
                  size = 3.5,
  ) +
  #add a smooth line fitting cpoe + epa
  #stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Cuota de Pases (%)",
       y = "Distancia promedio al objetivo (Yardas)",
       title = "Distancia promedio por pase vs cuota de pases para los 30 WRs más buscados",
       subtitle = "Semana: 1 | Temporada: 2021",
       caption = "Datos: @nflfastR | Figura: @Landeros_p33") +
  #uses the black and white ggplot theme
  theme_ft_rc() + # Dark theme based on FT’s dark theme
  scale_fill_ft() +
  #center title with hjust = 0.5
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5,
                              face = "bold",
                              family="Trebuchet MS"),
    plot.subtitle = element_text(size = 12,hjust = 0.5,family="Trebuchet MS"),
    axis.title.y = element_text(hjust = 0.5,
                                size = 10,
                                face = "bold",
                                family="Trebuchet MS"),
    axis.title.x = element_text(hjust = 0.5,
                                face = "bold",
                                size = 10,
                                family="Trebuchet MS")
    #axis.line=element_blank()
  ) +
  #make ticks look nice
  #if this doesn't work, `install.packages('scales')`
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("wrtargets.png",height = 6, width = 8, dpi=600)

# grafica TES =============
te <- joined_wr %>% 
  filter(position %in% c("TE")) %>% 
  dplyr::group_by(posteam,receiver_id, receiver) %>%
  dplyr::summarize( targets = n(),
                    air_yards = sum(air_yards),
                    yards_gained = sum(yards_gained)) %>%
  left_join(total_passes) %>% 
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr')) %>% 
  mutate(ts = targets / passes,
         adot = air_yards / targets) %>% 
  dplyr::arrange(-air_yards) %>% 
  head(25)

te %>%
  ggplot(aes(x = ts, y = adot)) +
  #horizontal line with mean EPA
  geom_hline(yintercept = mean(wr$adot), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept =  mean(wr$ts), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the right colors
  #cex controls point size and alpha the transparency (alpha = 1 is normal)
  #geom_point(color = wr$team_color) +
  #geom_image(aes(image = team_logo_espn), asp = 16 / 9) +
  geom_image(aes(image = team_logo_espn),
             size = 0.04,
             ##by="height",
             asp = 16 / 9 )+
  scale_size_identity()+
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=receiver),
                  family="Trebuchet MS",
                  colour = "#e8e8e8",
                  force_pull = 0.3,
                  box.padding = 0.6,
                  verbose = TRUE,
                  seed = 123,
                  max.time = 1,
                  max.iter = Inf,
                  size = 3.5,
  ) +
  #add a smooth line fitting cpoe + epa
  #stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Cuota de Pases (%)",
       y = "Distancia promedio al objetivo (Yardas)",
       title = "Distancia promedio por pase vs cuota de pases para los 25 TEs más buscados",
       subtitle = "Semana: 1 | Temporada: 2021",
       caption = "Datos: @nflfastR | Figura: @Landeros_p33") +
  #uses the black and white ggplot theme
  theme_ft_rc() + # Dark theme based on FT’s dark theme
  scale_fill_ft() +
  #center title with hjust = 0.5
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, hjust = 0.5,
                              face = "bold",
                              family="Trebuchet MS"),
    plot.subtitle = element_text(size = 12,hjust = 0.5,family="Trebuchet MS"),
    axis.title.y = element_text(hjust = 0.5,
                                size = 10,
                                face = "bold",
                                family="Trebuchet MS"),
    axis.title.x = element_text(hjust = 0.5,
                                face = "bold",
                                size = 10,
                                family="Trebuchet MS")
    #axis.line=element_blank()
  ) +
  #make ticks look nice
  #if this doesn't work, `install.packages('scales')`
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("tetargets.png",height = 6, width = 8, dpi=600)
  
