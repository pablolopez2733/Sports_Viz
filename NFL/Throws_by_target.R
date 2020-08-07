library(purrr)
library(dplyr)

seasons <- 2019
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/legacy-data/play_by_play_{x}.rds")
    )
  )
})


id_pos<-readRDS(
  url(("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/roster-data/roster.rds"))) %>% 
  dplyr::select(
    receiver_position=teamPlayers.position, 
    receiver_id = teamPlayers.gsisId) %>% distinct %>% filter(!is.na(receiver_id),!duplicated(receiver_id))


pbp_merged<-dplyr::left_join(pbp,id_pos,by='receiver_id',na_matches='never')


