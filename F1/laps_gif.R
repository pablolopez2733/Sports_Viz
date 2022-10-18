########################### F1 Analytics #######################################

# Libraries --------------------------------
library(devtools)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(gifski)
library(readr)
library(ggdark)

# Race Params --------------------------------
race_no <- 1
year <- 2021

# Load API wrapper:
devtools::source_url("https://gist.github.com/psychemedia/11187809/raw/31f8deab586322514b375ef581e83c909ab12d3c/ergastR-core.R")


# Create driver_data for 2021 season-----------------------------------------
# Load color catalogue
team.colors <- read_csv("data/f1_col_cat.csv")

# Team- driver catalogue
res <- resultsData.df(year,race_no)
driver_team_cat <- res %>% 
  select(driverId,constructorId) %>% 
  left_join(team.colors,by = "constructorId")



drivers <- driversData.df(year)
drivers <- drivers %>% 
  left_join(driver_team_cat,by = "driverId")
write.csv(drivers,
          "data/driver_data.csv",
          row.names = FALSE)



# Load data------------------------------------------------
drivers <- read_csv("data/driver_data.csv")
# Get laps data for every driver on the specified year and race number:
laps <- lapsData.df(year,race_no)
# Add mazepin:
mazepin <- c(1,"mazepin",20,0,0,0,0)
laps <- as.data.frame(rbind(laps,mazepin))

# Merge both dataframes to have the code, name and number.
df <- merge(laps,drivers,by = "driverId")

# Prepare data for plot -------------------
p <- df %>% mutate(position = as.numeric(position),
                   lap = as.numeric(lap))
p$pos_code <- paste0(p$position,".-",p$code)
asp_ratio <- 1.618


# Plot/gif --------------------------------
lap_plot <- p %>%
  select(-c(strtime,rawtime,cuml,diff)) %>% 
  ggplot(aes(x = lap, y = position, group = driverId)) +
  geom_segment(aes(xend = 56, yend = position), linetype = 2, color = 'grey') +
  geom_line(aes(color = team_color), alpha = 0.7, size = 2) +
  geom_label(aes(label = pos_code,colour = team_color))+
  dark_theme_gray() +
  theme(
    plot.title = element_text(size = 15, family = "Trebuchet MS",color = "red",hjust = 0.5),
    plot.subtitle = element_text(size = 12, family = "Trebuchet MS",color = "white",hjust = 0.5),
    axis.title = element_text(size = 8, family = "Trebuchet MS",color = "grey"),
    plot.caption = element_text(size=10, family = "Trebuchet MS",color = "grey",hjust = 0),
    legend.text = element_text(size = 5, family = "Trebuchet MS",color = "grey"),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    aspect.ratio = asp_ratio,
    axis.title.x = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, color = "white")) +
  scale_x_continuous(breaks = seq(0, 56, 4)) +
  scale_color_identity(aesthetics = c("color", "fill")) +
  coord_cartesian(clip = "off") + 
  transition_reveal(lap) + 
  labs(title = 'F1 BAHRAIN GP POSITIONS BY LAP',
       subtitle = "Season: 2021 | Race Number: 1",
       x = "Lap",
       y = "Position",
       caption = "Data: Ergast | Plot: @Landeros_p33")+
  scale_y_reverse(breaks = scales::pretty_breaks(n = 20))
  
gif <- animate(lap_plot,
        duration = 15,
        end_pause = 4,
        fps = 20,
        width = 1000,
        height = 1000, 
        renderer = gifski_renderer())

anim_save("C:/Users/MX03921586/Documents/Analytics/plots/laps_gif.gif", animation = gif)
  