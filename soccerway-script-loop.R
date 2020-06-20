library(tidyverse)
library(lubridate)
while(1) {
  start_time=Sys.time()
  ans=str_c("Source started at ", start_time)
  print(ans)
  source("soccerway-script.R")
  end_time=Sys.time()
  ans=str_c("Source completed at ", end_time)
  print(ans)
  script_options=readRDS("rds/soccerway_script_options.rds")
  sleep_hours=script_options$sleep_hours
  duration_seconds=as.duration(end_time-start_time)/dseconds(1)
  sleep_time_0=60*60*sleep_hours-duration_seconds # convert from seconds; coordinate with fraction got in script
  next_start=Sys.time()+sleep_time_0
  msg=str_c("Next start at ", next_start)
  print(msg)
  sleep_time=ifelse(sleep_time_0<=0, 0, sleep_time_0)
  Sys.sleep(sleep_time)
}
print(Sys.time())