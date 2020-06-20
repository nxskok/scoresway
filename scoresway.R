## ----one, message=F------------------------------------------------------
library(tidyverse)
library(rvest)
library(anytime)
library(lubridate)
library(RCurl)
source("functions.R")


## rm rds/mci*.rds


## ----two-----------------------------------------------------------------
xx=readRDS("rds/xx.rds")



## ------------------------------------------------------------------------
xx %>% filter(str_detect(score, " - ")) %>% 
  distinct(comp) %>% arrange(comp) %>% View("comps to get")
xx %>% filter(str_detect(score, " - ")) %>% 
  distinct(comp) %>% 
  arrange(comp) %>% 
  mutate(what=map(comp,~matches_by_id(.,"round"))) %>% 
  mutate(long=map_int(what, ~length(.))) %>% 
  filter(long==2) %>% 
  mutate(country=map_chr(what,"country"),
         matches=map(what,"matches")) %>% 
  unnest(matches) -> out_there
out_there %>% anti_join(games,by=c("matches"="match")) %>% arrange(matches) -> j
View(j, "matches to get")
j %>%
  select(match=matches,country) %>% 
  get_games(prefix="sched") -> xy # then combine, above; here is where the sched needs to be checked
xy
stopifnot(nrow(xy)>0)
xy = xy %>% mutate(score=ifelse(str_detect(score," - "),"upcoming",score)) 
xy
saveRDS(xy,"rds/xx.rds")
xy %>% count(country) %>% arrange(desc(n)) %>% print(n=Inf)
xy %>% arrange(time_stamp) %>% View("matches")
beepr::beep(2)


## ----three---------------------------------------------------------------
#last=tibble(comp=16,last_done=as.Date(0,origin="1970-01-01"))
last_mci=readRDS("rds/last_mci.rds")


## rm rds/mci*.rds


## ------------------------------------------------------------------------
min_get=1
max_get=400
today=Sys.time()
today
dtoday=as.Date(today)
back=3
forward=7
tibble(mydates=seq(dtoday-back,dtoday+forward,"days")) %>% 
  mutate(comps=map(mydates,date_comps)) %>% 
  unnest() %>% 
  distinct(comps) %>% 
  pull(comps) -> dc
dc
last_mci=readRDS("rds/last_mci.rds")
now=Sys.time()
tibble(comp=sort(dc)) %>% 
  left_join(last_mci) %>% 
  mutate(is_na=is.na(last_date)) %>% 
  mutate(random=runif(length(dc))) %>% 
  arrange(desc(is_na),last_date,random) %>% 
  mutate(r=row_number()) %>% 
  mutate(days_ago=(now-last_date)/ddays(1)) -> z
z %>% 
  slice(min_get:max_get) %>% 
  filter(days_ago>1) %>% 
  select(comp) -> xxx
xxx
stopifnot(nrow(xxx)>0)
z2 = xxx %>% mutate(last_date=Sys.time()) %>% 
  bind_rows(last_mci) %>% 
  group_by(comp) %>% 
  summarize(last_date=max(last_date))
z2
saveRDS(xxx,"rds/xxx.rds")
saveRDS(z2,"rds/last_mci.rds")
xxx=readRDS("rds/xxx.rds")
xxx %>% 
  mutate(matches=map(comp,matches_by_comp_id)) %>% 
  mutate(istib=map_lgl(matches,is.tibble)) -> dd2 # this distinguishes lists from others
xxx %>% 
  mutate(matches=map(comp,matches_by_comp_id)) %>% 
  mutate(istib=map_lgl(matches,is.tibble)) -> dd2 # this distinguishes lists from others
dd2
dd2
last_mci
dd25 = dd2 %>% filter(istib) %>% left_join(last_mci,by=c("comp"="comp")) %>% 
  arrange(!is.na(last_date),last_date)
dd25 %>% count(last_date)
dd25
dd3 = dd25 # %>% slice(1:100) # change this number as appropriate
dd3 = dd3 %>% mutate(last_date=today)
dd3
last = dd3 %>% select(comp,last_date) %>% 
  bind_rows(last_mci) %>% group_by(comp) %>% summarize_all(first)
thing = dd3 %>% 
  unnest() %>% 
  anti_join(games,by=c("matches"="match")) %>% 
  mutate(match=matches) %>% 
  arrange(match)
saveRDS(thing,"rds/thing.rds")
thing
thing %>% get_games(prefix="sched") -> xx # then combine, above; here is where the sched needs to be checked
xx
xx = xx %>% mutate(score=ifelse(str_detect(score," - "),"upcoming",score)) 
xx
saveRDS(xx,"rds/xx.rds")
xx %>% count(country) %>% arrange(desc(n)) %>% print(n=Inf)
xx %>% arrange(time_stamp) %>% View("matches")
beepr::beep(2)


## ------------------------------------------------------------------------
min_get=1
max_get=5


## ------------------------------------------------------------------------
xxx = z %>% 
  slice(min_get:max_get) %>% 
  select(comp)
z2 = xxx %>% mutate(last_date=Sys.time()) %>% 
  bind_rows(last_mci) %>% 
  group_by(comp) %>% 
  summarize(last_date=max(last_date))
saveRDS(xxx,"rds/xxx.rds")
saveRDS(z2,"rds/last_mci.rds")


## ---- warning=F----------------------------------------------------------
xxx=readRDS("rds/xxx.rds")
xxx %>% 
  mutate(matches=map(comp,matches_by_comp_id)) %>% 
  mutate(istib=map_lgl(matches,is.tibble)) -> dd2 # this distinguishes lists from others
xxx %>% 
  mutate(matches=map(comp,matches_by_comp_id)) %>% 
  mutate(istib=map_lgl(matches,is.tibble)) -> dd2 # this distinguishes lists from others
dd2
beepr::beep(2)


## ------------------------------------------------------------------------
dd2
last_mci
dd25 = dd2 %>% filter(istib) %>% left_join(last_mci,by=c("comp"="comp")) %>% 
  arrange(!is.na(last_date),last_date)
dd25 %>% count(last_date)
dd25
dd3 = dd25 # %>% slice(1:100) # change this number as appropriate
dd3 = dd3 %>% mutate(last_date=today)
dd3

#games=readRDS("rds/games.rds")
# update last

last = dd3 %>% select(comp,last_date) %>% 
  bind_rows(last_mci) %>% group_by(comp) %>% summarize_all(first)


thing = dd3 %>% 
  unnest() %>% 
  anti_join(games,by=c("matches"="match")) %>% 
  mutate(match=matches) %>% 
  arrange(match)
saveRDS(thing,"rds/thing.rds")
thing


## ----warning=F-----------------------------------------------------------
thing=readRDS("rds/thing.rds")
thing
thing %>% get_games(prefix="sched") -> xx # then combine, above; here is where the sched needs to be checked
xx
xx = xx %>% mutate(score=ifelse(str_detect(score," - "),"upcoming",score)) 
xx
saveRDS(xx,"rds/xx.rds")
xx %>% count(country) %>% arrange(desc(n)) %>% print(n=Inf)
xx %>% arrange(time_stamp) %>% View("matches")
beepr::beep(2)


## rm [12]*.html


## ----four-gg1------------------------------------------------------------
# gg=results_to_get(games,4.0)
min_hours=2.5
max_hours=3

gg=results_to_get(games,max_hours/24,since_hours = min_hours)
#if (nrow(gg)>1000) gg=sample_n(gg,1000)
gg = gg %>% arrange(time_stamp)
# View(gg,"matches to get")
saveRDS(gg,"rds/gg.rds")


## ----five,warning=FALSE--------------------------------------------------
gg=readRDS("rds/gg.rds")
gg %>% arrange(match) -> gg
View(gg, "matches to get")
gg %>% get_games() -> xx
saveRDS(xx,"rds/xx.rds")
#xx=get_games(gg)
xx %>% select(-(1:3)) %>% filter(str_detect(score," - ")) %>% unite(comp_full,country,comp_name) %>% 
  arrange(time_stamp) %>% View("results")
# xx %>% filter(str_detect(score," - ")) %>% count(country) %>% arrange(desc(n)) 
xx %>% mutate(is_done=str_detect(score, " - ")) %>% 
  count(is_done)
beepr::beep(2)


## ---- six-update-database------------------------------------------------
games=readRDS("rds/games.rds")
xx=readRDS("rds/xx.rds")
xx
games %>% mutate(is_res=has_result(score)) %>%  count(is_res) -> d1
games=combine2(games,xx) 
saveRDS(games,"rds/games.rds")
d = games %>% mutate(is_res=has_result(score) ) %>%  count(is_res) %>% bind_rows(d1, .id="which")
d %>% spread(is_res, n)


## rm [12]*.html


## ----seven---------------------------------------------------------------
now=Sys.time()
games %>% filter(score!="Cancelled", !(comp %in% c(43429,43430,49540,49542))) %>% 
  filter(!str_detect(score," - ")) %>% 
  filter(t1>0) %>% 
  mutate(since_ko=as.period(now-time_stamp)/hours(1)) %>% 
  filter(since_ko>3) %>%
#  mutate(since_ret=(now-retrieved)/ddays(1)) %>% 
#  filter(since_ret>0.75) %>% 
  mutate(num=as.period(now-time_stamp)/hours(1),
         den=as.period(retrieved-time_stamp)/hours(1),
         ratio=as.numeric(num)/as.numeric(den)) %>%
  filter(ratio<0 | ratio>2) %>% 
  arrange(ratio) %>% 
  mutate(row=row_number()) -> gg
saveRDS(gg,"rds/gg.rds")


## rm [12]*.html


## ----eight---------------------------------------------------------------
now=Sys.time()
(games %>% filter(score=="upcoming") %>% 
  filter(t1>0) %>% 
  mutate(since=as.duration(now-time_stamp)/dminutes(1)) %>%
  mutate(avail=(since>180)) %>% 
  filter(avail) %>% 
  arrange(retrieved) -> gg)


## ------------------------------------------------------------------------
n1get=1
n2get=2000
n2get=ifelse(n2get>nrow(gg), nrow(gg), n2get)
n2get
gg %>% slice(n1get:n2get) -> gg
saveRDS(gg,"rds/gg.rds")


## ----nine----------------------------------------------------------------
nrow(games)
games %>% filter(comp<=0) -> reget
games %>% filter(comp>0) %>% saveRDS("rds/games.rds")
games=readRDS("rds/games.rds") 
games %>% nrow()


## rm [12]*.html


## ------------------------------------------------------------------------
reget %>% pull(match) %>% enframe()


## ------------------------------------------------------------------------
reget %>% get_games() -> xx
saveRDS(xx,"rds/xx.rds")
xx


## ------------------------------------------------------------------------
xx %>% count(comp>0)


## rm [12]*.html


## ----ten-----------------------------------------------------------------
my_since=2.5 # days
now=Sys.time()
games %>% filter(score=="TBA") %>% 
  mutate(since=(now-retrieved)/ddays(my_since)) %>% 
  filter(since>my_since) -> tbas
tbas %>% pull(match) %>% enframe()


## ------------------------------------------------------------------------
tbas %>% get_games() -> xx
saveRDS(xx,"rds/xx.rds")
xx %>% mutate(what=case_when(
  score=="TBA" ~ "tba",
  str_detect(score, ":") ~ "kickoff",
  TRUE ~ "score"
)) %>% count(what)


## ----eleven--------------------------------------------------------------
games %>% mutate(diff=(retrieved-time_stamp)/dhours(1)) %>% 
  mutate(has_result=str_detect(score," - ")) %>% 
  mutate(since=case_when(
    !has_result ~ 1e10,
    TRUE        ~ diff
  )) %>% 
  mutate(score=ifelse(since<2.5, "00:16", score)) %>% 
  select(-diff, -has_result, -since) -> games
saveRDS(games, "rds/games.rds")


## ------------------------------------------------------------------------
games %>% filter(score=="00:16") %>% select(country:retrieved)


## ------------------------------------------------------------------------
games %>% filter(comp==41514)


## ------------------------------------------------------------------------
games %>% filter(comp==41245) %>% group_by(t1) %>% 
  count(t1_name) %>% arrange(desc(n)) %>% top_n(1)


## ------------------------------------------------------------------------
games %>% filter(comp==41506) %>% filter(!str_detect(score," - ")) %>% get_games() -> xx
xx


## ------------------------------------------------------------------------
games=readRDS("rds/games.rds")


## ------------------------------------------------------------------------
ll=lev2
ll=dc

n_back=10
tibble(id=ll) %>% mutate(first=map_dbl(id,first_match,games)) %>% 
  filter(!is.na(first)) %>% 
  mutate(matches=map## get upcoming games

get xx (games that I just got results for): this could stand to be shrunk to the games I actually *got* results for, not just the ones I might have done


## rm rds/mci*.rds


## ------------------------------------------------------------------------
xx=readRDS("rds/xx.rds")
xx %>% filter(str_detect(score, " - ")) %>% 
  distinct(comp) %>% nrow()


## ------------------------------------------------------------------------
xx %>% filter(str_detect(score, " - ")) %>% 
  distinct(comp) %>% mutate(what=map(comp,~matches_by_id(.,"round"))) %>% 
  mutate(long=map_int(what, ~length(.))) %>% 
  filter(long==2) %>% 
  mutate(country=map_chr(what,"country"),
         matches=map(what,"matches")) %>% 
  unnest(matches) -> out_there
out_there %>% anti_join(games,by=c("matches"="match")) %>% arrange(matches) -> j
j


## ------------------------------------------------------------------------
j %>%
  select(match=matches,country) %>% 
  get_games(prefix="sched") -> xy # then combine, above; here is where the sched needs to be checked
xy
stopifnot(nrow(xy)>0)
xy = xy %>% mutate(score=ifelse(str_detect(score," - "),"upcoming",score)) 
xy
saveRDS(xy,"rds/xx.rds")
xy %>% count(country) %>% arrange(desc(n)) %>% print(n=Inf)
xy %>% arrange(time_stamp) %>% View("matches")
beepr::beep(2)


## ------------------------------------------------------------------------
#last=tibble(comp=16,last_done=as.Date(0,origin="1970-01-01"))
last_mci=readRDS("rds/last_mci.rds")


## rm rds/mci*.rds


## ------------------------------------------------------------------------
today=Sys.time()
today
dtoday=as.Date(today)

tibble(mydates=seq(dtoday-3,dtoday+7,"days")) %>% 
  mutate(comps=map(mydates,date_comps)) %>% 
  unnest() %>% 
  distinct(comps) %>% 
  pull(comps) -> 
dc
last_mci=readRDS("rds/last_mci.rds")
z=tibble(comp=sort(dc)) %>% 
  left_join(last_mci) %>% 
  mutate(is_na=is.na(last_date)) %>% 
  mutate(random=runif(length(dc))) %>% 
  arrange(desc(is_na),last_date,random) %>% 
  mutate(r=row_number())
View(z, "leagues with games")
beepr::beep(2)


## ------------------------------------------------------------------------
min_get=1
max_get=49


## ------------------------------------------------------------------------
xxx = z %>% 
  slice(min_get:max_get) %>% 
  select(comp)
z2 = xxx %>% mutate(last_date=Sys.time()) %>% 
  bind_rows(last_mci) %>% 
  group_by(comp) %>% 
  summarize(last_date=max(last_date))
saveRDS(xxx,"rds/xxx.rds")
saveRDS(z2,"rds/last_mci.rds")


## ---- warning=F----------------------------------------------------------
xxx=readRDS("rds/xxx.rds")
xxx %>% 
  mutate(matches=map(comp,matches_by_comp_id)) %>% 
  mutate(istib=map_lgl(matches,is.tibble)) -> dd2 # this distinguishes lists from others
xxx %>% 
  mutate(matches=map(comp,matches_by_comp_id)) %>% 
  mutate(istib=map_lgl(matches,is.tibble)) -> dd2 # this distinguishes lists from others
dd2
beepr::beep(2)


## ------------------------------------------------------------------------
dd2
last_mci
dd25 = dd2 %>% filter(istib) %>% left_join(last_mci,by=c("comp"="comp")) %>% 
  arrange(!is.na(last_date),last_date)
dd25 %>% count(last_date)
dd25
dd3 = dd25 # %>% slice(1:100) # change this number as appropriate
dd3 = dd3 %>% mutate(last_date=today)
dd3

#games=readRDS("rds/games.rds")
# update last

last = dd3 %>% select(comp,last_date) %>% 
  bind_rows(last_mci) %>% group_by(comp) %>% summarize_all(first)


thing = dd3 %>% 
  unnest() %>% 
  anti_join(games,by=c("matches"="match")) %>% 
  mutate(match=matches) %>% 
  arrange(match)
saveRDS(thing,"rds/thing.rds")
thing


## ----warning=F-----------------------------------------------------------
thing=readRDS("rds/thing.rds")
thing
thing %>% get_games(prefix="sched") -> xx # then combine, above; here is where the sched needs to be checked
xx
xx = xx %>% mutate(score=ifelse(str_detect(score," - "),"upcoming",score)) 
xx
saveRDS(xx,"rds/xx.rds")
xx %>% count(country) %>% arrange(desc(n)) %>% print(n=Inf)
xx %>% arrange(time_stamp) %>% View("matches")
beepr::beep(2)


## rm [12]*.html


## ----gg2-----------------------------------------------------------------
# gg=results_to_get(games,4.0)
min_hours=2.5
max_hours=3
gg=results_to_get(games,max_hours/24,since_hours = min_hours)
#if (nrow(gg)>1000) gg=sample_n(gg,1000)
gg = gg %>% arrange(time_stamp)
View(gg,"matches to get")
saveRDS(gg,"rds/gg.rds")


## ----xx3,warning=FALSE---------------------------------------------------
gg=readRDS("rds/gg.rds")
gg %>% get_games() -> xx
saveRDS(xx,"rds/xx.rds")
#xx=get_games(gg)
xx %>% select(-(1:3)) %>% filter(str_detect(score," - ")) %>% unite(comp_full,country,comp_name) %>% 
  arrange(time_stamp) %>% View("results")
# xx %>% filter(str_detect(score," - ")) %>% count(country) %>% arrange(desc(n)) 
xx %>% mutate(is_done=str_detect(score, " - ")) %>% 
  count(is_done)
beepr::beep(2)


## ------------------------------------------------------------------------
games


## rm rds/mci*.rds


## ------------------------------------------------------------------------
xx=readRDS("rds/xx.rds")
xx %>% filter(str_detect(score, " - ")) %>% 
  distinct(comp) %>% nrow()


## ------------------------------------------------------------------------
xx %>% filter(str_detect(score, " - ")) %>% 
  distinct(comp) %>% mutate(what=map(comp,~matches_by_id(.,"round"))) %>% 
  mutate(long=map_int(what, ~length(.))) %>% 
  filter(long==2) %>% 
  mutate(country=map_chr(what,"country"),
         matches=map(what,"matches")) %>% 
  unnest(matches) -> out_there
out_there %>% anti_join(games,by=c("matches"="match")) %>% arrange(matches) -> j
j


## ------------------------------------------------------------------------
j %>%
  select(match=matches,country) %>% 
  get_games(prefix="sched") -> xy # then combine, above; here is where the sched needs to be checked
xy
stopifnot(nrow(xy)>0)
xy = xy %>% mutate(score=ifelse(str_detect(score," - "),"upcoming",score)) 
xy
saveRDS(xy,"rds/xx.rds")
xy %>% count(country) %>% arrange(desc(n)) %>% print(n=Inf)
xy %>% arrange(time_stamp) %>% View("matches")
beepr::beep(2)


## ------------------------------------------------------------------------
#last=tibble(comp=16,last_done=as.Date(0,origin="1970-01-01"))
last_mci=readRDS("rds/last_mci.rds")


## rm rds/mci*.rds


## ------------------------------------------------------------------------
today=Sys.time()
today
dtoday=as.Date(today)

tibble(mydates=seq(dtoday-3,dtoday+7,"days")) %>% 
  mutate(comps=map(mydates,date_comps)) %>% 
  unnest() %>% 
  distinct(comps) %>% 
  pull(comps) -> 
dc
last_mci=readRDS("rds/last_mci.rds")
z=tibble(comp=sort(dc)) %>% 
  left_join(last_mci) %>% 
  mutate(is_na=is.na(last_date)) %>% 
  mutate(random=runif(length(dc))) %>% 
  arrange(desc(is_na),last_date,random) %>% 
  mutate(r=row_number())
View(z, "leagues with games")
beepr::beep(2)


## ------------------------------------------------------------------------
min_get=1
max_get=49


## ------------------------------------------------------------------------
xxx = z %>% 
  slice(min_get:max_get) %>% 
  select(comp)
z2 = xxx %>% mutate(last_date=Sys.time()) %>% 
  bind_rows(last_mci) %>% 
  group_by(comp) %>% 
  summarize(last_date=max(last_date))
saveRDS(xxx,"rds/xxx.rds")
saveRDS(z2,"rds/last_mci.rds")


## ---- warning=F----------------------------------------------------------
xxx=readRDS("rds/xxx.rds")
xxx %>% 
  mutate(matches=map(comp,matches_by_comp_id)) %>% 
  mutate(istib=map_lgl(matches,is.tibble)) -> dd2 # this distinguishes lists from others
xxx %>% 
  mutate(matches=map(comp,matches_by_comp_id)) %>% 
  mutate(istib=map_lgl(matches,is.tibble)) -> dd2 # this distinguishes lists from others
dd2
beepr::beep(2)


## ------------------------------------------------------------------------
dd2
last_mci
dd25 = dd2 %>% filter(istib) %>% left_join(last_mci,by=c("comp"="comp")) %>% 
  arrange(!is.na(last_date),last_date)
dd25 %>% count(last_date)
dd25
dd3 = dd25 # %>% slice(1:100) # change this number as appropriate
dd3 = dd3 %>% mutate(last_date=today)
dd3

#games=readRDS("rds/games.rds")
# update last

last = dd3 %>% select(comp,last_date) %>% 
  bind_rows(last_mci) %>% group_by(comp) %>% summarize_all(first)


thing = dd3 %>% 
  unnest() %>% 
  anti_join(games,by=c("matches"="match")) %>% 
  mutate(match=matches) %>% 
  arrange(match)
saveRDS(thing,"rds/thing.rds")
thing


## ----warning=F-----------------------------------------------------------
thing=readRDS("rds/thing.rds")
thing
thing %>% get_games(prefix="sched") -> xx # then combine, above; here is where the sched needs to be checked
xx
xx = xx %>% mutate(score=ifelse(str_detect(score," - "),"upcoming",score)) 
xx
saveRDS(xx,"rds/xx.rds")
xx %>% count(country) %>% arrange(desc(n)) %>% print(n=Inf)
xx %>% arrange(time_stamp) %>% View("matches")
beepr::beep(2)


## rm [12]*.html


## ----gg3-----------------------------------------------------------------
# gg=results_to_get(games,4.0)
min_hours=2.5
max_hours=3
gg=results_to_get(games,max_hours/24,since_hours = min_hours)
#if (nrow(gg)>1000) gg=sample_n(gg,1000)
gg = gg %>% arrange(time_stamp)
View(gg,"matches to get")
saveRDS(gg,"rds/gg.rds")


## ----xx1,warning=FALSE---------------------------------------------------
gg=readRDS("rds/gg.rds")
gg %>% get_games() -> xx
saveRDS(xx,"rds/xx.rds")
#xx=get_games(gg)
xx %>% select(-(1:3)) %>% filter(str_detect(score," - ")) %>% unite(comp_full,country,comp_name) %>% 
  arrange(time_stamp) %>% View("results")
# xx %>% filter(str_detect(score," - ")) %>% count(country) %>% arrange(desc(n)) 
xx %>% mutate(is_done=str_detect(score, " - ")) %>% 
  count(is_done)
beepr::beep(2)


## ------------------------------------------------------------------------
d=tribble(
  ~x, ~y,
  1, "a",
  1, NA,
  1, "a",
  2, "b",
  2, NA,
  2, "b",
  3, NA
)
d %>%  arrange(x,y) %>% group_by(x) %>% fill(y)

games %>% arrange(comp,country) %>% group_by(comp) %>% fill(country) -> ggg
ggg %>% count(country) %>% print(n=Inf)
games=ggg

games %>% group_by(t1) %>% count(country) %>% filter(is.na(country))
games %>% group_by(t1) %>% count(country) %>% filter(t1 %in% c(78,83,87,89))


## ------------------------------------------------------------------------
all_names=function(id,games) {
  games %>% filter(t1==id) %>% count(t1_name)
}
id=1290
all_names(id,games)
games %>% filter(t1==id) %>% arrange(time_stamp) %>% print(n=Inf)

games
games %>% group_by(t1) %>% count(t1_name) %>% count(t1) %>% filter(nn>1) %>% print(n=Inf) %>% 
  mutate(names=map(t1,all_names,games)) %>% unnest() %>% print(n=Inf)


## ------------------------------------------------------------------------
leagues %>% filter(level==99) %>% pull(comp) -> dc
tibble(comp=dc) %>% mutate(matches=map(comp,matches_by_id_tibble,"round")) %>%  # print(n=Inf)
  unnest() %>% 
  anti_join(games,by=c("matches"="match")) %>% 
  mutate(match=matches) %>% get_games() -> xx # then combine, above
xx %>% select(-(1:3))
xx %>% count(country) %>% arrange(n) %>% print(n=Inf)


## ------------------------------------------------------------------------
games


## ------------------------------------------------------------------------
suggested = games %>%  arrange(match) %>% 
  mutate(diff=match-lag(match)) %>% 
  mutate(diffl=lead(match)-match) %>% 
  mutate(md=pmax(diff,diffl)) %>% 
  select(match,diff,diffl,md) %>% 
  mutate(suggest=case_when(
    diff>1 ~ match-1,
    diffl>1 ~ match+1,
    TRUE ~ NA_real_)) %>% 
  filter(suggest>2000000)
View(suggested)


## rm rds/sched*.rds


## ------------------------------------------------------------------------
nsamp=1000
gg = suggested %>% mutate(country=NA_character_) %>% 
  mutate(gotmatch=match,
         match=suggest) %>% 
  sample_n(nsamp) 
gg
saveRDS(gg,"rds/suggested.rds")


## ------------------------------------------------------------------------
gg %>% 
  get_games(prefix="sched") %>% 
  filter(t1>0)
xx
saveRDS(xx,"rds/xx.rds")


## ------------------------------------------------------------------------
games=combine2(games,xx) 
saveRDS(games,"games.rds")


## ------------------------------------------------------------------------
games %>% group_by(comp,comp_name) %>% count(country)


## ------------------------------------------------------------------------
games %>% filter(comp==41506) %>% arrange(desc(time_stamp))


## ------------------------------------------------------------------------
games %>% filter(match!=2473979) -> games


## ------------------------------------------------------------------------
saveRDS(games,"rds/games.rds")


## ------------------------------------------------------------------------
g2=readRDS("rds/games.rds")
g2 %>% filter(comp==41514)


## ------------------------------------------------------------------------
names(games)


## ------------------------------------------------------------------------
match = games %>% select(match,t1,t2,comp,score,time_stamp,retrieved)


## ------------------------------------------------------------------------
team1=games %>% select(t1,t1_name) %>% rename(id=t1,name=t1_name)
team1
team2=games %>% select(t2,t2_name) %>% rename(id=t2,name=t2_name)
team=bind_rows(team1,team2)
team


## ------------------------------------------------------------------------
comp = games %>% select(comp,comp_name,country)


## ------------------------------------------------------------------------
comp_country = comp %>% group_by(comp) %>% count(country) %>% arrange(desc(n)) %>% top_n(1) %>% select(-n)


## ------------------------------------------------------------------------
comp_name = comp %>% group_by(comp) %>% count(comp_name) %>% arrange(desc(n)) %>% top_n(1) %>% select(-n)
comp_name


## ------------------------------------------------------------------------
team_name = team %>% group_by(id) %>% count(name) %>% arrange(desc(n)) %>% top_n(1) %>% select(-n)


## ------------------------------------------------------------------------
names(games)
games = match %>% left_join(team_name,by=c("t1"="id")) %>% rename(t1_name=name) %>% 
  left_join(team_name,by=c("t2"="id")) %>% rename(t2_name=name) %>% 
  left_join(comp_country) %>% 
  left_join(comp_name)
saveRDS(games,"rds/games.rds")


## ----fixup---------------------------------------------------------------
saveRDS(games,"rds/games2.rds")
match = games %>% select(match,t1,t2,comp,score,time_stamp,retrieved)
team1=games %>% select(t1,t1_name) %>% rename(id=t1,name=t1_name)
team2=games %>% select(t2,t2_name) %>% rename(id=t2,name=t2_name)
team=bind_rows(team1,team2)
comp = games %>% select(comp,comp_name,country)
comp_country = comp %>% group_by(comp) %>% count(country) %>% arrange(desc(n)) %>% top_n(1) %>% select(-n)
comp_name = comp %>% group_by(comp) %>% count(comp_name) %>% arrange(desc(n)) %>% top_n(1) %>% select(-n)
team_name = team %>% group_by(id) %>% count(name) %>% arrange(desc(n)) %>% top_n(1) %>% select(-n)
games = match %>% left_join(team_name,by=c("t1"="id")) %>% rename(t1_name=name) %>% 
  left_join(team_name,by=c("t2"="id")) %>% rename(t2_name=name) %>% 
  left_join(comp_country) %>% 
  left_join(comp_name)
saveRDS(games,"rds/games.rds")


## ------------------------------------------------------------------------
games %>% filter(t1==1242) %>% arrange(time_stamp)


## ------------------------------------------------------------------------
game_list=2801777:2801802
game_list


## ------------------------------------------------------------------------
games %>% mutate(score=ifelse(match %in% game_list, "upcoming", score)) %>% 
  saveRDS("rds/games.rds")


## ------------------------------------------------------------------------
games %>% filter(comp==48274)


## ------------------------------------------------------------------------
country="England"
matches=2801777:2801802
games %>%  filter(match %in% matches)


## ------------------------------------------------------------------------
thing=tibble(country=country, match=matches)
thing
saveRDS(thing, "rds/thing.rds")


## ------------------------------------------------------------------------
games %>% filter(comp==48274) %>% arrange(desc(time_stamp)) %>% 
  filter(between(match, 2801979, 2802000))


## ------------------------------------------------------------------------
games %>% mutate(score=case_when(
  comp==48274 & between(match, 2801977, 2802000) ~ "17:00",
  TRUE                                           ~ score
)) -> games


## ------------------------------------------------------------------------
saveRDS(games, "rds/games.rds")


## ------------------------------------------------------------------------
dc=c(119)

