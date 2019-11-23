library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)

API_BASE <- "https://numeric.linards.net/"
httr::set_config(httr::config(http_version = 0))

dates <- GET(paste0(API_BASE, "data/dates")) %>%
  content("text") %>%
  fromJSON() %>%
  .$Dates %>%
  mutate(
    max_date = ymd_hms(max_date)
    , min_date = ymd_hms(min_date)
  )

