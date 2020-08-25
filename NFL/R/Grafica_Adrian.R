seasons <- 2019
pbp1 <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.rds")
    )
  )
})

library(dplyr)
library(ggplot2)
library(ggthemes)
library(extrafont)
loadfonts(device = "win")

dal<-pbp1 %>% filter(
  play_type %in% c('pass','run'),
  !is.na(epa),
  !is.na(wp),
  posteam == 'DAL',
  season_type=='REG') %>%
  mutate(
    wp = round(wp,2)
  )%>%group_by(wp) %>%
  summarise(
    epa = mean(epa),
  ) %>% ungroup() %>% mutate(
    group='DAL',
    level = 'team',
  )%>% filter(
    wp>=.10,
    wp<=.90)

phi<-pbp1 %>% filter(
  play_type %in% c('pass','run'),
  !is.na(epa),
  !is.na(wp),
  posteam == 'PHI',
  season_type=='REG') %>%
  mutate(
    wp = round(wp,2)
  )%>%group_by(wp) %>%
  summarise(
    epa = mean(epa),
  ) %>% ungroup() %>% mutate(
    group='PHI',
    level = 'team',
  )%>% filter(
    wp>=.10,
    wp<=.90)

was<-pbp1 %>% filter(
  play_type %in% c('pass','run'),
  !is.na(epa),
  !is.na(wp),
  posteam == 'WAS',
  season_type=='REG') %>%
  mutate(
    wp = round(wp,2)
  )%>%group_by(wp) %>%
  summarise(
    epa = mean(epa),
  ) %>% ungroup() %>% mutate(
    group='WAS',
    level = 'team',
  )%>% filter(
    wp>=.10,
    wp<=.90)

nyg<-pbp1 %>% filter(
  play_type %in% c('pass','run'),
  !is.na(epa),
  !is.na(wp),
  posteam == 'NYG',
  season_type=='REG') %>%
  mutate(
    wp = round(wp,2)
  )%>%group_by(wp) %>%
  summarise(
    epa = mean(epa),
  ) %>% ungroup() %>% mutate(
    group='NYG',
    level = 'team',
  )%>% filter(
    wp>=.10,
    wp<=.90)

nfl<-pbp1 %>% filter(
  play_type %in% c('pass','run'),
  !is.na(epa),
  !is.na(wp),
  season_type=='REG')  %>%
  mutate(
    wp = round(wp,2)
  )%>%
  group_by(wp) %>%
  summarise(
    epa = mean(epa)
  ) %>% ungroup()%>% mutate(
    group='Promedio de NFL',
    level = 'nfl',
  ) %>% filter(
    wp>=.10,
    wp<=.90)

plot<-rbind(dal,phi,was,nyg,nfl)


colors<-nflfastR::teams_colors_logos
colors %>% select(team_abbr,team_color,team_color2,team_color3,team_color4) %>% filter(team_abbr=='NYG') #cambia el equipo
# aqui manualmente veia el color de cada equipo, porque con unos use el color 2, con phily el 1 y con Dallas el 3

plot$group<-factor(plot$group, levels = c("DAL","PHI","WAS","NYG","Promedio de NFL"))
plot$level<-factor(plot$level, levels = c('team','nfl'))

plot %>% ggplot(aes(x=wp,y=epa)) + 
  geom_smooth(aes(color=group,linetype=level,alpha=alph),se=F,size=.55,alpha=.7)+
  theme_fivethirtyeight() +
  scale_color_manual(values = c('#acc0c6',"#004953",'#ffb612',"#0b2265","#a71930" ))+
  scale_linetype_discrete(name="level",
                          breaks=c("team","team","team","team","nfl"),
                          labels= NULL,guide=FALSE)+
  theme(
    plot.title = element_text(size = 8, family = "Trebuchet MS",color = "grey20",hjust = 0),
    plot.subtitle = element_text(size = 5.5, family = "Trebuchet MS",color = "grey20",hjust = 0),
    axis.title = element_text(size = 5.5, family = "Trebuchet MS",color = "grey20"),
    plot.caption = element_text(size=4.5, family = "Trebuchet MS",color = "grey20",hjust = 0),
    legend.text = element_text(size = 5, family = "Trebuchet MS",color = "grey20"),
    legend.title = element_blank(),
    legend.direction = "horizontal",
    legend.position = "top",
    legend.key.size =unit(.75,"line"),
    axis.text = element_text(size = 3.5, family = "Trebuchet MS",color = "grey20"),
    panel.grid = element_line(size=.11)) +
  labs(x='Probabilidad de Victoria', y='Eficiencia Ofensiva (EPA/Jugada)',
       title = "Ofensiva de Cowboys Empeoraba Cuando el Juego Estaba Cerrado",
       subtitle = 'Eficiencia ofensiva promedio por probabilidad de victoria | Temporada 2019',
       caption = "Probabilidad de victoria entre 10% and 90% | Jugadas de pase y corrida | Temporada Regular | 2019
Datos: nflfastR | Gráfica por Adrian Cadena @adrian_cadem" )+ 
  annotate(
    geom = "curve", x = .56, y = .4,xend = .5, yend = .1, 
    curvature = -.1, size= unit(.15, "mm"),arrow = arrow(length = unit(1.3, "mm")),
  )+
  annotate(geom = "label", x = .565, y = .43,size=1.5,family = "Trebuchet MS",color = "grey20",fill="#F0F0F0",
           label = 'Situación de Juego Cerrado')+
  xlim(.1, .9)+ coord_cartesian(ylim = c(-.28, .45)) 

ggsave('/Desktop/wp_esp.png', dpi=1500, width = 11, height = 8, units = "cm")