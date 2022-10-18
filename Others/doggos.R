
# Libraries --------------------------------
library(devtools)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(gifski)
library(readr)
library(ggdark)
library(ggdogs)
library(janitor)
library(fuzzyjoin)
library(ggrepel)

load_data <- function(path) { 
  files <- dir(path, pattern = '\\.csv', full.names = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}
# Leemos datos y data cleaning -------------------------------------------------
path <- paste0(getwd(),"/data/")
name <- "akc_breed_info.csv"
size <- read.csv(paste0(path,name)) %>% 
  clean_names()

path <- paste0(getwd(),"/data/")
name <- "dog_intelligence.csv"
intel <- read.csv(paste0(path,name)) %>% 
  clean_names()
# Checamos cuales nos faltan
missing <- intel %>% 
  fuzzy_anti_join(size,match_fun = str_detect)

size$height_low_inches <- as.numeric(size$height_low_inches)
size$height_high_inches <- as.numeric(size$height_high_inches)
size$weight_low_lbs <- as.numeric(size$weight_low_lbs)
size$weight_high_lbs <- as.numeric(size$weight_high_lbs)
size <- size %>% 
  filter(!is.na(height_high_inches))
size$height=(size$height_low_inches + size$height_high_inches )/ 2
size$weight=(size$weight_low_lbs + size$weight_high_lbs)/ 2
size <- size %>% 
  filter(!is.na(height_high_inches)) 
size$imc = size$weight * size$height
df <- intel %>% 
  fuzzy_left_join(size,match_fun = str_detect) %>% 
  filter(breed.x != "Vizsla")
df$obey<-gsub("%","",as.character(df$obey))
df$obey <- as.numeric(df$obey)
df$obey <- ifelse(is.na(df$obey),0,df$obey)

df <- df %>% 
  mutate(dog = case_when(
    classification =="Brightest Dogs" ~ "doge_strong",
    classification =="Excellent Working Dogs" ~ "glasses",
    classification =="Above Average Working Dogs" ~ "husky",
    classification =="Average Working/Obedience Intelligence" ~ "tail",
    classification =="Fair Working/Obedience Intelligence" ~ "gabe",
    classification =="Lowest Degree of Working/Obedience Intelligence " ~ "doge",
  ))

# PLOT ------------------------------------------------------------------------
ggplot(df, aes(log(height), obey,label = breed.x)) +
  geom_dog(aes(dog = dog), size = 1.9, asp = 1) +
  geom_label_repel(min.segment.length = 0,
                   size=3,
                   vjust=0.5,
                   max.overlaps = 13) +
  geom_vline(xintercept = 2.89,color = "black")+
  geom_hline(yintercept = 50, color = "black") +
  labs(x = "Altura",
       y = "Inteligencia (0-100)",
       title = "DOGGO ANALYTICS:",
       subtitle = "Tamaño vs Inteligencia en razas",
       caption = "Datos: DATA.WORLD \n Gráfica: @Landeros_p33") +
  annotate(geom = "label",x=2.2, y= 13,
           size = 4, color = "gold2", fill = "black",
           label = "Pequeños y tontos") +
  annotate(geom = "label",x=2.2, y= 110,
           size = 4, color = "gold2", fill = "black",
           label = "Pequeños pero inteligentes") +
  annotate(geom = "label",x=3.25, y= 110,
           size = 4, color = "gold2", fill = "black",
           label = "Grandes e inteligentes") +
  annotate(geom = "label",x=3.25, y= 13,
           size = 4, color = "gold2", fill = "black",
           label = "Grandes pero tontos") +
  theme_minimal() +
  theme(aspect.ratio = .9,
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(size = 16,hjust = 0.5),
        plot.subtitle = element_text(size = 12,hjust = 0.5))+
  xlim(1.5,4)+
  ylim(-10,120)+
  scale_x_continuous()+
  scale_y_continuous()
ggsave(paste0(getwd(),"/plots/dog_plot.png"),height = 8,width = 7)

# Pug--------------------------------------------------
install.packages("remotes")
remotes::install_github("R-CoderDotCom/ggdogs@main")
library(ggdogs)
library(tidyverse)
grid <- expand.grid(1:5, 3:1)
df <- data.frame(x = grid[, 1],
                 y = grid[, 2])
df$dog <- "pug"
ggplot(df) +
  geom_dog(aes(x, y, dog = dog), size = 5) +
  xlim(c(0.25, 5.5)) + 
  ylim(c(0.25, 3.5)) +
  ggtitle("Pugs")+
  theme(plot.title = element_text(size = 16,hjust = 0.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(x = "Altura",
       y = "Inteligencia (0-100)")+
  annotate(geom = "label",x=3, y= 3.5,
           size = 5, color = "gold2", fill = "black",
           label = "Feos y troncos") 
