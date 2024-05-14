require(httr)
require(tidyverse)
require(lubridate)



#  Define the Bering Sea dataset
httr::content(httr::GET('https://apex.psmfc.org/akfin/data_marts/akmp/bs_crab_avg_sst?start_date=19850101&end_date=20501130'), type = "application/json") %>% 
  bind_rows %>% 
  mutate(date=as_date(READ_DATE)) %>% 
  data.frame %>% 
  dplyr::select(date,meansst=MEANSST,crabarea=CRAB_AREA)%>%
  mutate(read_date=date,
         month=month(read_date),
         day=day(read_date),
         year=year(read_date),
         newdate=as.Date(ifelse(month==12,as.character(as.Date(paste("1999",month,day,sep="-"),format="%Y-%m-%d")),#  Create a dummy year so that each year can more easily be overlain
                                as.character(as.Date(paste("2000",month,day,sep="-"),format="%Y-%m-%d"))),format("%Y-%m-%d")),
         year2=ifelse(month==12,year+1,year)) %>% # To have our years go from Dec-Nov, force December to be part of the subsequent year.
  arrange(read_date) %>%
  saveRDS("/srv/shiny-server/bs-crab-avg-sst/data/crabsst.RDS")
  #saveRDS("data/crabsst.RDS")


  