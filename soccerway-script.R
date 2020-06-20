# script version of from_soccerway.Rmd, designed to be run as job
#
# packages and setup
#
library(tidyverse)
library(rvest)
library(lubridate)
source("functions.R")
games=readRDS("rds/games.rds")
script_options=readRDS("rds/soccerway_script_options.rds")
frac_to_sample=script_options$frac_to_sample # fraction of leagues to sample
# ans <- str_c("sampling fraction: ", frac_to_sample)
# print(ans)
#
games %>% group_by(comp) %>% summarize(last=max(retrieved)) -> last_got
omits=c(50839, 54473, 50408, 50409, 54468, 54472, 54467, 54474, 54469, 54475, 54470, 55641, 55639)
today=Sys.Date()
rate <- rate_delay(0.5)
# leagues_from_date_slow=slowly(leagues_from_date, rate)
leagues_from_date_slow=slowly(leagues_from_date, rate) 
# seq(today+50, today+80, by=1) %>% enframe(value="date_date") %>%
seq(today-7, today+3, by=1) %>% enframe(value="date_date") %>%
  mutate(date=format(date_date, "%Y/%m/%d")) %>% 
  mutate(d=map(date, ~leagues_from_date_slow(.))) %>% 
  unnest() %>% 
  select(league_url, comp_id) %>% 
  distinct() -> d
# print("Obtained leagues from date")
d %>% filter(!comp_id %in% omits) %>% 
  left_join(last_got,by=c("comp_id"="comp")) %>% 
  mutate(hours=(Sys.time()-last)/dhours(1)) %>% 
  sample_frac(frac_to_sample) -> d # get this fraction of leagues each time; coordinate with frequency of get
# this is leagues to get now
# get them
rate <- rate_delay(0.2)
pb <- progress_estimated(nrow(d)) # old, replare with pb <- progress_bar$new(format = " progress [:bar] :percent eta: :eta", total = nrow(l))
scores_from_league_slow=slowly(scores_from_league, rate)
d %>% mutate(scores=map2(league_url, comp_id, ~scores_from_league_slow(.x, .y, pb))) %>% 
  unnest(scores) %>% select(-league_url, -comp_id) -> today_games
# print("done getting scores from league")
# print(today_games)
# update database
count_types=function(g) {
  g %>% mutate(score_is=case_when(
    str_detect(score, " - ") ~ "score",
    TRUE                   ~ "no score"
  )) %>% count(score_is)
}
tab1=count_types(games)
combine2(games, today_games) -> games
games %>% distinct(match, .keep_all = T) -> games
saveRDS(games,"rds/games.rds")
# print("Done updating database 1")
# save summary table
tab2=count_types(games)
tab1 %>% left_join(tab2, by="score_is") %>% 
  mutate(change=n.y-n.x) %>% saveRDS("rds/summary.rds")
# remove to-soons
games %>% mutate(diff=(retrieved-time_stamp)/dhours(1)) %>% 
  mutate(has_result=str_detect(score," - ")) %>% 
  mutate(since=case_when(
    !has_result ~ 1e10,
    TRUE        ~ diff
  )) %>% 
  mutate(score=ifelse(since<2.5, str_replace(score, "-", ":"), score)) %>% 
  select(-diff, -has_result, -since) -> games
saveRDS(games, "rds/games.rds")
# print("Done updating database 2")
