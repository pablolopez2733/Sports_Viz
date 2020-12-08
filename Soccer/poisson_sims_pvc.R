library(dplyr)
library(ggplot2)
library(ggimage)


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
simulacion <- sim_matches(10000)
tabla <- as.data.frame(table(simulacion$avanza))

#para animar:
simulacion <- simulacion %>%
  mutate(vic_pumas = cumsum(unam.w),
         vic_caz = cumsum(caz.w))


#graficas-----------------------------------------------------
ggplot(simulacion,aes(x=avanza))+
  geom_bar(stat="count", width=0.7, fill=c("#2331e5","#cbab57"))+
  theme_minimal()


#gif:------------------------------------------------------------------------
pumas <- as.data.frame(cbind(simulacion$iteracion,simulacion$vic_pumas))
caz <- as.data.frame(cbind(simulacion$iteracion,simulacion$vic_caz))
pumas$logo <- "https://upload.wikimedia.org/wikipedia/en/thumb/2/25/UNAM_Pumas.svg/800px-UNAM_Pumas.svg.png"
caz$logo <- "https://upload.wikimedia.org/wikipedia/commons/thumb/3/38/Escudo_del_Cruz_Azul_AC.svg/800px-Escudo_del_Cruz_Azul_AC.svg.png"

#hacerlo




# Save at gif:
anim_save("288-animated-barplot-transition.gif")