library(dplyr)
library(ggplot2)
library(ggimage)
library(tidyverse)
library(gganimate)
library(gifski)
library(janitor)
library(shadowtext)

#Pumas vs azul
#adjusting for h2h record:
set.seed(33)
goles <- c(0,1,2,3,4,5,6,7)
g_pumas <- dpois(goles, 2.422508)
g_caz <- dpois(goles, 2.694507644)

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
simulacion <- sim_matches(100)
tabla <- as.data.frame(table(simulacion$avanza))

#para animar:
simulacion <- simulacion %>%
  mutate(vic_pumas = cumsum(unam.w),
         vic_caz = cumsum(caz.w))



#gif:------------------------------------------------------------------------
pumas <- as.data.frame(cbind(simulacion$iteracion,simulacion$vic_pumas))
caz <- as.data.frame(cbind(simulacion$iteracion,simulacion$vic_caz))
pumas$logo <- "#303af1"
caz$logo <- "cbab57"
pumas$equipo <- "UNAM"
caz$equipo <- "CAZ"

df_gif <- as.data.frame(rbind(pumas,caz))
colnames(df_gif) <- c("iteracion","wins","logo","equipo")

cols.num <- c("iteracion","wins")
df_gif[cols.num] <- sapply(df_gif[cols.num],as.numeric)
sapply(df_gif, class)

#con menos numeros
iters <- seq(from = 0, to = 100, by = 5)
r_df <- df_gif[df_gif$iteracion %in% iters,]


#PLOT
ggplot(data = df_gif,
       mapping = aes(x=iteracion,
                     y=wins,
                     group = equipo))+
  geom_point()+
  scale_x_continuous(limits = c(0,max(df_gif$iteracion)))+
  shadowtext::geom_shadowtext(data = df_gif,
                              mapping = aes(x=iteracion,
                                            y=wins,
                                            label = equipo),
                              hjust=-0.1, vjust = 0, bg.color = "black")+
  coord_cartesian(clip = "off") +
  transition_reveal(iteracion)
#colors "#cbab57" "#303af1"
