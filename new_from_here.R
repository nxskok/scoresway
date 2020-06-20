
# or possibly simplify

url_to_game_possibly=possibly(url_to_game, otherwise=list())

new_from_here <- function(games_to_get) {
  if (nrow(games_to_get)==0) {
    return(games_to_get)
  }
  games_to_get %>% 
    mutate(match_info=map(match_url, ~url_to_game_possibly(.))) %>% 
    unnest(match_info, .name_repair="minimal") -> d
  if (nrow(d)==0) {
    print("Empty data frame")
    return(d)
  }
  d %>%
    select(-match_url, -date, -match1, -.name_repair, -league_url) -> mmm
  print("done go")
  mmm
}


view_nice <- function(mmm, message) {
  if (nrow(mmm)==0) {
    print("no rows")
    return()
  }
  mmm %>% mutate(since=(retrieved-time_stamp)/dhours(1)) %>% 
    mutate(score=ifelse(stat != "FT", str_replace(score, "-", ":"), score)) %>%   
    select(-since) -> mmm
  mmm %>% mutate(status=case_when(
    str_detect(score, " - ") ~ "done",
    str_detect(score, " : ") ~ "in progress",
    str_detect(score, ":")   ~ "scheduled",
    TRUE                     ~ "zzz other"
  )) %>%
    arrange(status, time_stamp) %>% View(message)
}

make_combine <- function(mmm) {
  if (nrow(mmm)==0) {
    print("No rows to combine")
    return()
  }
  games <- read_rds("rds/games.rds")
  games <- combine2(games, mmm)
  games %>% distinct(match, .keep_all = T) -> games
  saveRDS(games,"rds/games.rds")
  games
}

new_combine <- function(games_to_get, message) {
  mmm <- new_from_here(games_to_get)
  view_nice(mmm, message)
  make_combine(mmm)
}