library(tidyverse)
library(stringr)
library(leaflet)
library(leaflet.providers)
library(htmlwidgets)
library(sf)
library(leaflet.extras)
library(tigris)
library(rmarkdown)
library(htmltools)
library(lubridate)

current_date_time <- as.POSIXct(as.numeric(Sys.time()), origin='1970-01-01', tz="EST")
yesterday_date_time <- current_date_time - 172800

current_date_time_pretty <- as.character(format(current_date_time, '%B %d, %Y - %I:%M %p %Z'))



#dots - past snowfall data
download.file("https://www.weather.gov/source/crh/lsr_snow.geojson", "snow.geojson")
snow <- st_read("snow.geojson")
snow_clean <- st_transform(snow, crs = 4326) %>% 
  mutate(valid_time_clean = as.POSIXct(as.numeric(valid_time), origin='1970-01-01', tz="EST")) %>% 
  mutate(valid_time_pretty = as.character(format(valid_time_clean, '%B %d, %Y - %I:%M %p %Z'))) %>% 
  filter(valid_time_clean >= yesterday_date_time) %>% 
  mutate(amount = as.numeric(amount)) %>% 
  mutate(color_bin = case_when(amount < 0.5 ~ "Less than half an inch",
                               (amount >= 0.5 & amount < 1) ~ "0.5-1 inches",
                               (amount >= 1 & amount < 3) ~ "1-3 inches",
                               (amount >= 3 & amount < 6) ~ "3-6 inches",
                               (amount >= 6 & amount < 9) ~ "6-9 inches",
                               (amount >= 9 & amount < 12) ~ "9-12 inches",
                               TRUE ~ "A foot or more")) %>% 
  mutate(remarks_new = case_when(remarks == "" ~ "None",
                                 TRUE ~ remarks))

snow_clean_df_nyc <- snow_clean %>% 
  st_drop_geometry() %>% 
  select(-color) %>% 
  filter(state == "NY" |state == "NYC" | state == "NJ" | state == "CT")
write.csv(snow_clean_df_nyc, "output/snow_clean_df_nyc.csv", row.names = FALSE)
