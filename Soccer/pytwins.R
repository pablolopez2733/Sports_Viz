library(readr)
library(ggplot2)
library(ggrepel)
library(ggimage)
library(here)
library(ggdark)

plpoints <- read_csv("C:/Users/pablo/Desktop/GithubRepos/Experiments/Data/plpoints.txt")

plpoints$ppg <- plpoints$Pts/38
plpoints$pytwins <- 2.499973*(plpoints$GF^(1.22777)/(plpoints$GA^(1.127248)+plpoints$GF^(1.072388)))
#rename for merge
names(plpoints)[2] <- "Team"
#Import crests
crests <- read_csv("C:/Users/pablo/Desktop/GithubRepos/9plus6/HomeAdvantage/Bases_de_Datos/crests.csv")
#merge
plpoints <- merge(plpoints,crests,by="Team")
#el modelo:
linmod <- lm(ppg ~ pytwins, data=plpoints)
summary(linmod)


#Plot
pywins <-
  ggplot(plpoints, aes(x = pytwins, y = ppg)) + 
  geom_point(size = 3) +
  stat_smooth(geom='line', alpha=0.8, se=FALSE, method='lm',colour="#fc56d0")+
  geom_image(aes(image = plpoints$url),asp=16/9)+
  xlim(0.4, 3.1)+
  ylim(0,2.5)+
  dark_theme_gray() +
  
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 14, family = "Calibri",color = "white",hjust = 0),
    plot.subtitle = element_text(size = 11, family = "Calibri",color = "white",hjust = 0),
    axis.title = element_text(size = 9, family = "Calibri",color = "#adadad"),
    plot.caption = element_text(size=8, family = "Serif",color = "#adadad",hjust = 0),
    legend.text = element_text(size = 8, family = "Calibri",color = "#adadad"),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.key.size =unit(.75,"line"),
    axis.text = element_text(size = 9, family = "Trebuchet MS",color = "#adadad"),
    panel.grid = element_line(size=.11))+
    labs(x='Forecast', y='Actual PPG',
       title = "Over/Under performers according to Pythagorean Expected PPG",
       subtitle = 'R^2 = 0.8941 | Premier League Season 2019-2020',
       caption = "Data: FB Reference | Plot by Pablo L. Landeros @Landeros_p33" )+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
pywins
ggsave(filename = here::here("/Desktop/GithubRepos/Experiments/Soccer/pytwins_plot.png"), 
       height = 6, width = 8)
