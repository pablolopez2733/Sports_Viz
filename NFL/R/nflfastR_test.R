#------------------------
#Prueba NFL FastR
#------------------------

#Load Pakages:
library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)

#Para no usar notación científica:
options(scipen = 9999)

#Load data (Not using nflfastR but pre-cleaned 2019 season data):
data <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2019.rds'))

#Ver las variables:
names(data)
#view(data)

#filtro para solo jugadas de pase o corrida:
data %>% 
  filter(rush == 1 | pass == 1) %>%
  select(posteam, desc, rush, pass, name, passer, rusher, receiver) %>% 
  head()

#Special Teams plays:
data %>% 
  filter(special == 1) %>%
  select(down, ydstogo, desc) %>% 
  head()

#Fouth down plays:
data %>% 
  filter(down == 4) %>%
  select(down, ydstogo, desc) %>% 
  head()

#Fourth down plays that arent special teams:
data %>% 
  filter(down == 4 & special == 0) %>%
  select(down, ydstogo, desc) %>% 
  head()

#now We store in a variable all run and passing plays with none missing EPA
pbp_rp <- data %>%
  filter(rush == 1 | pass == 1, !is.na(epa))

#Lets see how some Dallas Cowboys rushers

pbp_rp %>%
  filter(posteam == "DAL", rush == 1) %>%
  group_by(rusher) %>%
  summarize(
    mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()
  ) %>%
  arrange(-mean_epa) %>%
  filter(plays > 20) #Those rushers who rushed 20 plays or more

#Lets say we want to introduce a new column which will be equal to 1 if the home team has the ball
pbp_rp %>%
  mutate(
    home = if_else(posteam == home_team, 1, 0)
  ) %>%
  select(posteam, home_team, home) %>%
  head(10)

#Now if we want to create a column on 1 or more conditions:
pbp_rp %>%
  filter(!is.na(cp)) %>%
  mutate(
    depth = case_when(
      air_yards < 0 ~ "Negative",
      air_yards >= 0 & air_yards < 10 ~ "Short",
      air_yards >= 10 & air_yards < 20 ~ "Medium",
      air_yards >= 20 ~ "Deep"
    )
  ) %>%
  group_by(depth) %>%
  summarize(cp = mean(cp))

#FIGURE WITH QB STATS-----------------------------------------------------------------------------
seasons <- 2015:2019
pbp <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  )
})

qbs <- pbp %>%
  filter(week <= 17, !is.na(epa)) %>%
  group_by(id, name) %>%
  summarize(
    epa = mean(qb_epa),
    cpoe = mean(cpoe, na.rm = T),
    n_dropbacks = sum(pass),
    n_plays = n(),
    team = last(posteam)
  ) %>%
  ungroup() %>%
  filter(n_dropbacks > 100 & n_plays > 1000)

#join with team logos:
qbs <- qbs %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

#Plotting with team logos:
qbs %>%
  ggplot(aes(x = cpoe, y = epa)) +
  #horizontal line with mean EPA
  geom_hline(yintercept = mean(qbs$epa), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept =  mean(qbs$cpoe), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos
  geom_image(aes(image = team_logo_espn), size = qbs$n_plays / 45000, asp = 16 / 9) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=name)) +
  #add a smooth line fitting cpoe + epa
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Completion % above expected (CPOE)",
       y = "EPA per play (passes, rushes, and penalties)",
       title = "Quarterback Efficiency, 2015 - 2019",
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

#INTENTO: Most efficient Qbs in the last season
#hacer gráfica anterior pero para temporada 2019 y filtrando a jugadas en la yarda 20

#jalar datos:
qbs_redzone <- data %>%
  filter(week <= 17, pass ==1, ydstogo<=20, !is.na(epa)) %>%
  group_by(id, name) %>%
  summarize(
    epa = mean(qb_epa),
    cpoe = mean(cpoe, na.rm = T),
    n_dropbacks = sum(pass),
    n_plays = n(),
    team = last(posteam)
  ) %>%
  ungroup() %>%
  filter(n_dropbacks > 50 & n_plays > 200)

#Add logos:
qbs_redzone <- qbs_redzone %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

#Plot:--------------------------------------------------------------------
qbs_redzone %>%
  ggplot(aes(x = cpoe, y = epa)) +
  #horizontal line with mean EPA
  geom_hline(yintercept = mean(qbs_redzone$epa), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept =  mean(qbs_redzone$cpoe), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos
  geom_image(aes(image = team_logo_espn), size = qbs_redzone$n_plays/9500, asp = 16 / 9) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=name)) +
  #add a smooth line fitting cpoe + epa
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Completion % above expected (CPOE)",
       y = "EPA per play (only on passing plays)",
       title = "Quarterback Efficiency in the redzone (2019)",
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

#Plot 2: Ahora quiero ver si jala con nplays vs EPA------------------------------
qbs_redzone %>%
  ggplot(aes(x = n_plays, y = epa)) +
  #horizontal line with mean EPA
  geom_hline(yintercept = mean(qbs_redzone$epa), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept =  mean(qbs_redzone$n_plays), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos
  geom_image(aes(image = team_logo_espn), size = 0.10, by ="height", asp = 16 / 9) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=name)) +
  #add a smooth line fitting cpoe + epa
  stat_smooth(geom='line', alpha=0.10, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Number of plays in the red zone (only on passing plays)",
       y = "EPA per play",
       title = "Quarterback Efficiency in the redzone (2019)",
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
