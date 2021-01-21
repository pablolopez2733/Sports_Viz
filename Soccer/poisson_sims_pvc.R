library(dplyr)
library(ggplot2)
library(ggimage)
library(tidyverse)
library(gganimate)
library(gifski)
library(janitor)
library(shadowtext)
library(ggtext)
library(ggthemes)

#Pumas vs azul
#adjusting for h2h record:
set.seed(33)
goles <- c(0,1,2,3,4,5,6,7) #vector de goles a muestrear
g_pumas <- dpois(goles, 2.422508) #UNAM anota segun Po(2.42)
g_caz <- dpois(goles, 2.694507644) #CAZ anota segun Po(2.42)

#funcion para simular partidos---------------------------------------------
sim_matches <- function(n){
  p <- NULL
  c <- NULL
  iteracion <- NULL
  avanza <- NULL
  
  caz.w <- NULL
  unam.w <- NULL
  for (i in 1:n) {
    
    p[i] <- sample(goles,1,prob=g_pumas,replace = TRUE)
    c[i] <- sample(goles,1,prob=g_caz,replace = TRUE)
    iteracion[i] <- i
    
    if((c[i]==0 & p[i]== 4) |(c[i]==0 & p[i]==5)|(c[i]==0 & p[i]==6)|(c[i]==0 & p[i]==7)|(c[i]==1 & p[i]==6)|(c[i]==1 & p[i]==7)|(c[i]==2 & p[i]==7))
      avanza[i] <- "UNAM"
    else
      avanza[i] <- "CAZ"
    #para poder animarlo:
    if(avanza[i] == "UNAM")
    {
      unam.w[i] <- 1
      caz.w[i] <- 0
    }
    else
      if(avanza[i] == "CAZ")
      {
        caz.w[i] <- 1
        unam.w[i] <- 0
      }
    
  }
  
  sim <- as.data.frame(cbind(iteracion,p,c,unam.w,caz.w,avanza))
  return(sim)
  
}
#------------------------------------------------------------------------------

#simulamos 10000 veces
simulacion <- sim_matches(10000)
tabla <- as.data.frame(table(simulacion$avanza))

# para poder animar:
simulacion <- simulacion %>%
  mutate(vic_pumas = cumsum(unam.w),
         vic_caz = cumsum(caz.w))



# cosas necesarias para el gif:--------------------------------------------------
pumas <- as.data.frame(cbind(simulacion$iteracion,simulacion$vic_pumas))
caz <- as.data.frame(cbind(simulacion$iteracion,simulacion$vic_caz))
pumas$logo <- "#303af1"
caz$logo <- "#cbab57"
pumas$equipo <- "UNAM"
caz$equipo <- "CAZ"

df_gif <- as.data.frame(rbind(pumas,caz))
colnames(df_gif) <- c("iteracion","wins","logo","equipo")

cols.num <- c("iteracion","wins")
df_gif[cols.num] <- sapply(df_gif[cols.num],as.numeric)
sapply(df_gif, class)

#con menos numeros, no fue necesario alv
iters <- seq(from = 0, to = 10000, by = 500)
r_df <- df_gif[df_gif$iteracion %in% iters,]


#PLOT con escala logaritmica:------------------------------------------------------
ggplot(data = df_gif,
       mapping = aes(x=iteracion,
                     y=wins,
                     color = logo,
                     group = equipo))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,max(df_gif$iteracion)))+
  scale_y_log10(expand = expansion(add = c(0,0.1)), 
                breaks=c(20,50,100,150, 500, 1000, 2000, 5000,7000, 10000)) +
  theme_minimal() +
  scale_color_manual(values=c("#cbab57", "#303af1"))+
  shadowtext::geom_shadowtext(data = df_gif,
                              mapping = aes(x=iteracion,
                                            y=wins,
                                            label = equipo),
                              hjust=-0.1, vjust = 0, bg.color = "black")+
  theme(
    plot.title = element_text(size = 18, family = "Trebuchet MS",color = "grey20",hjust = 0),
    plot.subtitle = element_text(size = 13, family = "Trebuchet MS",color = "grey20",hjust = 0),
    axis.title = element_text(size = 10, family = "Trebuchet MS",color = "grey20"),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(3,15,3,3,"mm"),
    plot.caption = ggtext::element_markdown()
  )+
  coord_cartesian(clip = "off") +
  labs(x = "Numero de Simulacion", 
       y = "Pases a siguiente ronda (En escala logarítmica)",
       title = "Simulacion Monte Carlo de 10,000 partidos",
       subtitle =  "Posibilidades de pase de Cruz Azul ~ 40:1",
       caption = "<span style = 'font-size:8pt;color:#888888'>Data Source: FB REF <br> Plot: Pablo L. Landeros </span>") +
  transition_reveal(iteracion) 
#colors "#cbab57" "#303af1"
anim_save('C:/Users/pablo/Desktop/GithubRepos/Experiments/Soccer/pumas_v_azul_log.gif')

#Sin logaritmica:--------------------------------------------------------
ggplot(data = df_gif,
       mapping = aes(x=iteracion,
                     y=wins,
                     color = logo,
                     group = equipo))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,max(df_gif$iteracion)))+
  theme_fivethirtyeight() +
  scale_color_manual(values=c("#cbab57", "#303af1"))+
  shadowtext::geom_shadowtext(data = df_gif,
                              mapping = aes(x=iteracion,
                                            y=wins,
                                            label = equipo),
                              hjust=-0.1, vjust = 0, bg.color = "black")+
  theme(
    plot.title = element_text(size = 18, family = "Trebuchet MS",color = "grey20",hjust = 0),
    plot.subtitle = element_text(size = 13, family = "Trebuchet MS",color = "grey20",hjust = 0),
    axis.title = element_text(size = 12, family = "Trebuchet MS",color = "grey20"),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(3,15,3,3,"mm"),
    plot.caption = ggtext::element_markdown()
  )+
  coord_cartesian(clip = "off") +
  labs(x = "Numero de Simulacion", 
       y = "Pases a siguiente ronda",
       title = "Simulacion Monte Carlo de 10,000 partidos",
       subtitle =  "Probabilidad de pase de Cruz Azul: 97.8 %",
       caption = "<span style = 'font-size:8pt;color:#888888'>Data Source: FB REF <br> Plot: Pablo L. Landeros </span>") +
  transition_reveal(iteracion) 
#colors "#cbab57" "#303af1"
anim_save('C:/Users/pablo/Desktop/GithubRepos/Experiments/Soccer/pumas_v_azul.gif')