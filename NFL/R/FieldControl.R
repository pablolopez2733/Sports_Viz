library(tidyverse) # metapackage of all tidyverse packages
library(gganimate)
library(mvtnorm)
library(readr)

options(repr.plot.width=20, repr.plot.height = 10)
df_games <- read_csv("C:/Users/pablo/Desktop/games.csv")
df_plays <- read_csv("C:/Users/pablo/Desktop/plays.csv/plays.csv")

#Let´s select Andrew Lucks' big plays 
Luck_plays <- df_plays %>%
  filter(possessionTeam == "IND",
         stringr::str_detect(playDescription, "TOUCHDOWN"),
         stringr::str_detect(playDescription, "Luck")
         ,offensePlayResult >25)

#select A LUCK-TY connection for TD
play_ <- Luck_plays %>%
  select(gameId, playId, possessionTeam,
         playDescription, absoluteYardlineNumber, yardsToGo) %>% 
         slice(7)

game_ <- df_games %>%
  filter(gameId == play_$gameId)
#here, we see that out play was in week eleven


#load week eleven tracking:
week11 <- read_csv("C:/Users/pablo/Desktop/week11.csv")
df_track <- week11 %>% 
  filter(gameId == play_$gameId,playId == play_$playId)


play_direction_ <- df_track %>% head(1) %>% dplyr::pull(playDirection)
df_track <- df_track %>%
  dplyr::select(x, y, s, dir, event, displayName, jerseyNumber, frameId, team)

#Change to cartesian:
df_track <- df_track %>%
  dplyr::mutate(
    dir_rad = dir * pi / 180,
    v_x = sin(dir_rad) * s,
    v_y = cos(dir_rad) * s,
    v_theta = atan(v_y / v_x),
    v_theta = ifelse(is.nan(v_theta), 0, v_theta),
    team_name = case_when(
      team == "home" ~ game_$homeTeamAbbr,
      team == "away" ~ game_$visitorTeamAbbr,
      TRUE ~ team,
    )
  ) %>%
  dplyr::select(frameId, event, team = team_name,
                jerseyNumber, displayName,
                x, y, s, v_theta, v_x, v_y)
#So now we have the play data. Let´s animate it.

#-------------NOW WE ANIMATE IT--------------------------------
