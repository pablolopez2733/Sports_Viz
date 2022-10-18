library(tidyverse)
library(fastDummies)

juego_moneda <- function(n_tiros){
 roll <- sample(c(0,1), size = n_tiros, replace = TRUE)
 score <- sum(roll)
 return(score)
}

juego_dado <- function(n_tiros){
  score <-c()
  roll <- sample(1:6, size = n_tiros, replace = TRUE)
  for (i in 1:length(roll)) {
    score[i] <- ifelse(roll[i] == 6,1,0)
  }
  
  return(sum(score))
}

simulacion <- function(n,tiros_dado,tiros_moneda){
  
  mat = matrix(ncol = 3, nrow = 0)
  df=data.frame(mat)
  colnames(df) = c("nsim","score_moneda","score_dado")
  
  for (i in 1:n) {
    run <- c(i,
             juego_moneda(tiros_moneda),
             juego_dado(tiros_dado))
    df[nrow(df)+1,] <- run
  }
  res <- df %>% 
    mutate(winner = case_when( score_moneda < score_dado  ~ "dado",
                               score_moneda > score_dado  ~ "moneda",
                               TRUE ~ "empate"
                               )) %>% 
    
  return(res)
  
  
}
################################
# simular el juego 10,000 veces:
nsim <- 1000
tiros_moneda <- 4
tiros_dado <- 12
################################

juego <- simulacion(nsim,tiros_dado,tiros_moneda)
table(juego$winner)
prop.table(table(juego$winner))
var_dado <- var(juego$score_dado)
var_moneda <- var(juego$score_moneda)

# score de juegos:
ggplot(data = juego) +
  ggtitle(glue("{nsim} Simulaciones"))+
  geom_point(aes(x=nsim,y=score_moneda,color = "blue"))+
  geom_line(aes(x=nsim,y=score_moneda,color = "blue"))+
  geom_point(aes(x=nsim,y=score_dado,color = "red"))+
  geom_line(aes(x=nsim,y=score_dado,color = "red"))+
  xlab('Simulacion')+
  ylab('Score')+
  theme(plot.title = element_text(hjust = 0.5,size = 14)) +
  scale_color_identity(name = "Resultados",
                       breaks = c("blue", "red"),
                       labels = c("Moneda", "Dado"),
                       guide = "legend")

  
# cumscore 
resultados <- juego %>% 
  select(nsim,winner) %>% 
  fastDummies::dummy_cols(select_columns = c("winner")) %>% 
  mutate(cum_dado = cumsum(winner_dado),
         cum_empate = cumsum(winner_empate),
         cum_moneda = cumsum(winner_moneda))
# gráfica cumscore
ggplot(data = resultados) +
  ggtitle(glue("Victorias en {nsim} simulaciones"))+
  geom_line(aes(x=nsim, y=cum_moneda, color = "blue"))+
  geom_line(aes(x=nsim, y=cum_dado, color = "red"))+
  geom_line(aes(x=nsim, y=cum_empate, color = "green"))+
  xlab('Simulacion')+
  ylab('Wins')+
  theme(plot.title = element_text(hjust = 0.5,size = 14))+
  scale_color_identity(name = "Resultados",
                       breaks = c("blue", "red", "green"),
                       labels = c("Moneda", "Dado", "Empate"),
                       guide = "legend")

