# scoresway functions

games=readRDS("rds/games.rds")

match_search=function(i,n) {
  seq.int(i-n,i,length.out=n+1)
}

first_match=function(compn,g) {
  g %>% filter(comp==compn) %>% summarize(f=first(match)) %>% pull(f)
}

not_in=function(matchlist,games) {
  matchlist[!(matchlist %in% games$match)]
}

get_results=function(d,since_days) {
  d %>% 
    filter(!str_detect(score," - ")) %>% 
    filter(time_stamp<Sys.time()) %>% 
    mutate(since=Sys.time()-time_stamp) %>% 
    filter(since<ddays(since_days)) %>% # or a bit more
    filter(since>dhours(2.5)) %>% # finished
    select(match,country,t1_name,t2_name,score,time_stamp,since) -> x
  print(x)
  x %>% 
    get_games()
}

results_to_get=function(d,since_days,min_days=0, since_hours=2.5) {
  d %>% 
    filter(!str_detect(score," - ")) %>% 
    filter(time_stamp<Sys.time()) %>% 
    mutate(since=Sys.time()-time_stamp) %>% 
    filter(since<ddays(since_days)) %>% # or a bit more
    filter(since>ddays(min_days)) %>% 
    filter(since>dhours(since_hours)) %>% # finished
    arrange(time_stamp) %>% 
    select(match,country,t1_name,t2_name,score,time_stamp,since) 
}

get_which=function(since_days,d) {
  d %>% 
    filter(!str_detect(score," - ")) %>% 
    filter(time_stamp<Sys.time()) %>% 
    mutate(since=Sys.time()-time_stamp) %>% 
    filter(since<ddays(since_days)) %>% # or a bit more
    filter(since>dhours(2)) %>% # finished
    select(match,country,t1_name,t2_name,score,time_stamp,since) 
}

game_is_missing=function(games) {
  games %>% filter(!str_detect(score," - "))
}

back_n_league=function(ll,n,games) {
  print(ll)
  m2=matches_in(games,ll)
  if (length(m2)>0) {
    back_n(m2,n)
  } else {
    empty_match(0)
  }
}

back_n=function(j,n) {
  # j is a previous set of game numbers
  m=min(j)
  p=(m-n):(m-1)
  get_game_numbers(p)
}

get_game_numbers=function(j) {
  map_df(j,game_data,NA_character_)
}

missing_matches=function(games,comp) {
  mm=matches_in(games,comp)
  pp=min(mm):max(mm)
  pp[!(pp %in% mm)]
}

matches_in=function(games,this_comp) {
  games %>% filter(comp==this_comp) %>% pull(match)
}

read_html_safe=possibly(read_html,otherwise=NA)

## new combine: 

has_result=function(score) {
  ifelse(str_detect(score," - "),"score", "no score")
}

# m1 %>% anti_join(m2, by="thing") %>% bind_rows(m2) %>% distinct()

combine2=function(m1, m2) {
  m1 %>% anti_join(m2, by="match") %>% bind_rows(m2)
}

combine=function(m1,m2) {
  m2 %>% bind_rows(m1) %>% group_by(match) %>% summarize_all(first)
}


table_by_comp_id=function(id) {
  matches=matches_by_comp_id(id)
  map_df(matches$matches,game_data,matches$country)
}

table_by_comp_id_safe=safely(table_by_comp_id,otherwise=NA)

table_by_id=function(id,what) {
  matches=matches_by_id(id,what)
  print(matches)
  map_df(matches$matches,game_data,matches$country)
}

get_games=function(d,prefix="match") {
  print("In get_games")
  map2_df(d$match,d$country,game_data,prefix=prefix)
}


matches_by_id=function(id,what) {
  print(id)
  base_url_1="http://www.scoresway.com/?sport=soccer&page="
  base_url_1a="&id="
  base_url_2="&view=matches"
  url=paste0(base_url_1,what,base_url_1a,id,base_url_2)
  #print(url)
  html=read_html(url, options="RECOVER", verbose=T)
  if (is.na(html)) return(NA)
  html %>% html_nodes("table") %>% html_nodes("td") %>% html_nodes("a") %>% html_attr("href") -> hrefs
  # find the ones that refer to a match
  if (is.null(hrefs)) return(list(matches=0,country=""))
  if (length(hrefs)==0) {
    return(list(matches=0,country=""))
  }
  hrefs %>% str_split("&",simplify=T) %>%  as.data.frame() %>% 
    mutate(is_match=(V2=="page=match")) %>% 
    mutate(is_not_event=!str_detect(V3,"events$")) %>% 
    filter(is_match,is_not_event) %>% 
    mutate(id=extract_id(V3)) %>% pull(id) -> ids
  #print(ids)
  html %>% html_nodes("h2") %>% .[2] %>% html_text() -> country
  list(matches=unique(ids),country=country)
}

matches_by_id_tibble=function(id,what) {
  ll=matches_by_id(id,what)
  as_tibble(ll)
}

matches_by_comp_id=function(id) {
  print(paste("matches_by_comp_id",id))
  fname=paste0("rds/mci",id,".rds")
  if (file.exists(fname)) {
    return(readRDS(fname))
  }
  base_url_1="http://www.scoresway.com/?sport=soccer&page=competition&id="
  base_url_2="&view=matches"
  url=paste0(base_url_1,id,base_url_2)
  # create local filename, safe-download file, then read from **************************************
  html=read_html(url, options="RECOVER", verbose=T)
  Sys.sleep(0.5) # or maybe I need more
  if (is.na(html)) return(list(matches=0,country=""))
  html %>% html_nodes("table") %>% html_nodes("td") %>% html_nodes("a") %>% html_attr("href") -> hrefs
  # find the ones that refer to a match
  if (length(hrefs)==0) {
    return(list(matches=numeric(0),country=""))
  }
  hrefs %>% str_split("&",simplify=T) %>%  as.data.frame() %>% 
    mutate(is_match=(V2=="page=match")) %>% 
    mutate(is_not_event=!str_detect(V3,"events$")) %>% 
    filter(is_match,is_not_event) %>% 
    mutate(id=extract_id(V3)) %>% pull(id) -> ids
  html %>% html_nodes("h2") %>% .[2] %>% html_text() -> country
  mci=tibble(matches=unique(ids),country=country)
  saveRDS(mci,fname)
}


date_of_match=function(dds) { # get date and kickoff time
  dds[2] %>% html_nodes("a") %>% html_attr("href")
}

date_comps=function(the_date) { # get the competitions on this date
  print(paste("date_comps:",the_date))
  base_url="http://www.scoresway.com/?sport=soccer&page=matches&date="
  url=paste0(base_url,the_date)
  html=read_html(url, options="RECOVER", verbose=T)
  Sys.sleep(0.5)
  if (is.na(html)) return(NA)
  html %>% html_nodes("th") %>% html_nodes("a") %>% html_attr("href") -> today_comps
  extract_id(today_comps)
}

### safe_dl ##########################

safe_dl=possibly(~download.file(.x,.y,method="libcurl",quiet=3),"nogood")

get_game=function(mid) { # this reads the website
  match_base_url="http://www.scoresway.com/?sport=soccer&page=match&id="
  url=paste0(match_base_url,mid)
  html_file=str_c(mid,".html")
  #  print(str_c("Match ",mid," URL ",url))
  #  repeat {
  #    rawhtml <- try(getURL(url, .encoding="ISO-8859-1", .mapUnicode = F),silent=TRUE)
  #    print(class(rawhtml))
  #    if(class(rawhtml) != "try-error") break
  #    Sys.sleep(0.5)
  #  }
  #  rc=download.file(url,html_file,method="libcurl",quiet=3) # quiet suppresses message
  rc=safe_dl(url,html_file)
  if (rc==0) {
    x=read_html(html_file, options="RECOVER", verbose=T) # changed from rawhtml above
    print(paste(mid,rc))
    Sys.sleep(0.5)
    return(x)
  }
  NA
}

empty_match=function(match_number=-1) {
  # same format as tibble returned by game_data
  tibble(match=match_number,t1=0,t2=0,comp=-1,country="",t1_name="none",t2_name="none",score="none",
         comp_name="none",time_stamp=anytime(0,tz="America/Toronto"),retrieved=Sys.time())
}

game_data=function(mid,prefix="match",ctry="") { # if prefix=="sched" don't get score
  #  print(paste("game_data, ",prefix,mid))
  fname=paste0("rds/",prefix,mid,".rds")
  if (file.exists(fname)) {
    return(readRDS(fname))
  }
  html=get_game(mid)
  Sys.sleep(0.5)
  if (anyNA(html)) return(empty_match(mid))
  html %>% html_nodes("h3") -> game_info
  if (is.null(game_info)) return(empty_match(mid))
  if (length(game_info)==0) return(empty_match(mid))
  if (is.na(game_info)) return(empty_match(mid))
  # print(game_info)
  # 1 and 3 are teams, 2 is score or kickoff time
  # get id and team name
  txt=map_chr(game_info,game_text) # t1 name, score/ko, t2 name
  num=game_ids(game_info) # t1 id, t2 id
  html %>% html_nodes("dl") %>% html_nodes('dd') -> dds
  dds[1] %>% html_text() -> comp_text
  dds[1] %>% html_nodes("a") %>% html_attr("href") -> comp_info
  comp_id=extract_id(comp_info)
  dds[2] %>% html_nodes("span") %>% html_attr("data-value") -> time_stamp
  time_stamp=as.numeric(time_stamp)
  mm=tibble(match=as.integer(mid),t1=as.integer(num[1]),t2=as.integer(num[2]),
            comp=as.integer(comp_id),country=ctry,
            t1_name=txt[1],t2_name=txt[3],score=txt[2],comp_name=comp_text,
            time_stamp=anytime(time_stamp,tz="America/Toronto"),retrieved=Sys.time())
  saveRDS(mm,fname)
  mm
}

game_text=function(game_inf) {
  game_inf %>% html_text() %>% trimws()
}

extract_id=function(s) {
  s %>%  
    str_extract("id=\\d+") %>% 
    str_extract("\\d+") %>% 
    as.numeric()
}

game_ids=function(game_inf) {
  game_inf %>% html_nodes("a") %>% html_attr("href") -> info
  extract_id(info)
}


##### move to soccerway



# New functions to get matches from soccerway rather than scoresway (which no longer exists)

## leagues from date

leagues_from_date = function(date) {
  # date in form "yyyy/mm/dd"
  base = "https://www.soccerway.com"
  url = str_c(base, "/matches/", date, "/") #added 2020-02-08
  print(url)
  html <- read_html(url, options="RECOVER", verbose=T)
  html %>% html_nodes("a") %>%
    html_attr("href") %>% as_tibble() %>%
    mutate(is_r = str_detect(value, "r([0-9]+).$")) %>%
    filter(is_r) %>%
    mutate(league_url = str_c(base, value)) %>%
    mutate(comp_id1=str_extract(league_url, "r[0-9]+.$")) %>% 
    mutate(comp_id=as.numeric(str_extract(comp_id1, "[0-9]+"))) %>% 
    select(league_url, comp_id)
}


## games from league



matches_from_league=function(league_url) {
  html <- read_html(league_url, options="RECOVER", verbose=T)
  base <-  "https://www.soccerway.com"
  html %>% html_nodes("a") %>%
    html_attr("href") %>% as_tibble() %>%
    filter(str_detect(value, "matches")) %>% pluck(1, "value") -> matches_url
  read_html(str_c(base, matches_url), options="RECOVER", verbose=T) %>%
    html_nodes("tr") -> match_rows
  match_rows
}




get_match_info = function(match_row, league, comp_id) {
  match_row %>% html_attrs() -> my_attrs
  if (is.list(my_attrs)) my_attrs=my_attrs[[1]]
  match_row %>% html_nodes("td") %>% html_text() %>%
    str_replace_all("\n", "") %>% str_trim() -> row_as_text
  match_row %>% html_nodes("td") %>% 
    html_nodes("a") %>% 
    html_attr("title") -> td_attrs
  match_row %>% html_nodes("a") %>%
    html_attr("href") %>% as_tibble() %>%
    filter(str_detect(value, "^.teams")) %>%
    mutate(id_slash = str_extract(value, "[0-9]+.$")) %>%
    mutate(id = as.numeric(str_extract(id_slash, "[0-9]+"))) %>%
    pull(id) -> team_ids
  my_attrs %>%
    str_extract("[0-9]+$") -> nums
  enframe(league) %>% separate(
    value,
    sep = "/",
    into = c("base", "x0", "url", "what", "country", "comp", "year", "part")
  ) -> what
  timestamp = as_datetime(as.numeric(my_attrs[2]), tz = "America/Toronto")
  
  tibble(
    match = as.numeric(nums[3]),
    t1 = team_ids[1],
    t2 = team_ids[2],
    comp = comp_id,
    t1_name = td_attrs[1],
    t2_name = td_attrs[3],
    score = row_as_text[4],
    country = what$country,
    comp_name = what$comp,
    time_stamp = timestamp,
    retrieved = Sys.time()
  ) -> ans
  stuff=list(my_attrs, row_as_text, nums)
  ans
}


scores_from_league=function(url, id, pb) {
  # print(url) # if I am getting 404s, uncomment this line
  saveRDS(id, "id.rds")
  pb$tick()
  w <- matches_from_league(url)
  map_df(w, ~ get_match_info(., url, id)) %>%
    filter(str_detect(score, "-") | str_detect(score, ":") | str_detect(score, "TBA")) -> d
  # msg=str_c("obtained matches from ", url)
  # print(msg)
  d
}

scores_from_league2=function(url, id) {
  # print(url) # if I am getting 404s, uncomment this line
  saveRDS(id, "id.rds")
  w <- matches_from_league(url)
  map_df(w, ~ get_match_info(., url, id)) %>%
    filter(str_detect(score, "-") | str_detect(score, ":") | str_detect(score, "TBA")) -> d
  # msg=str_c("obtained matches from ", url)
  # print(msg)
  d
}

### 2019-11-15

# get game urls from a league match url

league_url_to_game_urls <- function(league_url) {
  Sys.sleep(0.5)
  league_url %>% read_html() %>% 
    html_nodes("a") %>% html_attr("href") %>% enframe() %>% 
    filter(str_detect(value, "matches")) %>% 
    filter(str_detect(value, "[0-9]/$")) %>% 
    rename(game_url=value)
}

# get game info from a (full) game url

url_to_game=function(url) {
  
  retrieved_time <- now()
  read_html(url) -> the_match
  Sys.sleep(0.5)
  
  # match number
  
  url %>% str_split("/") %>% pluck(1, 12) %>% as.numeric() -> match_number
  
  
  # team names and score
  
  xpaths <- c('//*[@id="page_match_1_block_match_info_5"]/div/div/div[1]/a[2]', # home team
              '//*[@id="page_match_1_block_match_info_5"]/div/div/div[3]/a[2]', # away tean
              '//*[@id="page_match_1_block_match_info_5"]/div/div/div[2]/h3/span[1]', # match status
              '//*[@id="page_match_1_block_match_info_5"]/div/div/div[2]/h3/text()', # score
              '//*[@id="page_match_1_block_match_info_5"]/div/div/div[5]/a[2]', # league
              '//*[@id="page_match_1_block_match_info_5"]/div/div/div[5]') # details, ko date and time
  
  the_match %>% html_nodes(xpath = xpaths[1]) %>% 
    html_text() -> t1_name
  the_match %>% html_nodes(xpath = xpaths[1]) %>% 
    html_attr("href") %>% 
    str_split("/") %>% pluck(1, 5) %>% as.numeric() -> t1_id
  the_match %>% html_nodes(xpath = xpaths[2]) %>% 
    html_text() -> t2_name
  the_match %>% html_nodes(xpath = xpaths[2]) %>% 
    html_attr("href") %>% 
    str_split("/") %>% pluck(1, 5) %>% as.numeric() -> t2_id
  
  the_match %>% html_nodes(xpath=xpaths[3]) %>% html_text() -> status
  
  the_match %>% html_nodes(xpath=xpaths[4]) %>% html_text() %>% 
    pluck(2) %>% str_remove_all("\n") %>% 
    str_trim() -> score
  
  the_match %>% html_nodes(xpath=xpaths[5]) %>% 
    html_text() -> league_name
  
  the_match %>% html_nodes(xpath=xpaths[5]) %>% 
    html_attr("href") %>% 
    str_split("/") -> comp_info
  
  comp_info %>% pluck(1, 7) %>% 
    str_remove("r") %>% as.numeric() -> league_number
  
  comp_info %>% pluck(1, 3) -> country
  
  the_match %>% html_nodes(xpath=xpaths[6]) %>% 
    html_nodes("span") %>% html_attr("data-value") %>% 
    enframe() %>% filter(!is.na(value)) %>% pull() %>% as.numeric() %>% 
    as_datetime(tz="America/Toronto") -> kickoff
  
  pb$tick()
  
  # tibble
  
  tibble(match=match_number,
         t1=t1_id,
         t2=t2_id,
         comp=league_number,
         country=country,
         t1_name=t1_name,
         t2_name=t2_name,
         score=score,
         comp_name=league_name,
         time_stamp=kickoff,
         retrieved=retrieved_time,
         stat=status
  )  
  



}

# get game info from (full) league (match) url

league_url_to_game_info <- function(league_url) {
  base="http://www.soccerway.com"
  # print(glue({format(now(), "%Y-%m-%d %H:%M:%S")}, ": ", {league_url}))
  league_url_to_game_urls(league_url) %>% 
    mutate(url_full=str_c(base, game_url)) %>% 
    select(url_full) %>% 
    mutate(match_info=map(url_full, ~url_to_game(.))) %>% 
    unnest(match_info) %>% 
    select(-url_full) -> d
  pb$tick()
  d
}

league_url_to_match_numbers <- function(league_url) {
  read_html(league_url) %>% html_nodes("a") %>% html_attr("href") %>% 
    enframe() %>% filter(str_detect(value, "^/matches/")) %>%
    extract(value, into="match", regex="/([0-9]+)/$", remove=F, convert=T) %>% 
    extract(value, into="date", regex="^/matches/([0-9]+/[0-9]+/[0-9]+)/", remove=F) %>% 
    drop_na(match) %>% 
    mutate(date=ymd(date)) %>% 
    distinct(match, .keep_all = T) %>% 
    select(-name, match_url=value) -> d
  pb$tick()
  d
}


