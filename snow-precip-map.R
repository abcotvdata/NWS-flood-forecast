library(tidyverse)
library(stringr)
library(leaflet)
library(leaflet.providers)
library(htmlwidgets)
library(colourvalues)
library(sf)
library(leaflet.extras)
library(tigris)
library(rmarkdown)
library(htmltools)
library(lubridate)

current_date_time <- as.POSIXct(as.numeric(Sys.time()), origin='1970-01-01', tz="EST")
yesterday_date_time <- current_date_time - 86400

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

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title {
    position: fixed !important;
    right: 0%;
    bottom: 6px;
    text-align: right;
    padding: 10px;
    background: rgba(255,255,255,0.75);
    font-style: italic;
    font-size: 10px;
    color: black;
  }
  @media only screen and (max-width: 460px) {
    .leaflet-control.map-title {
      font-size: 8px;
    }
  }
"))

title <- tags$div(
  tag.map.title, HTML("Source: <a href=\"https://www.weather.gov/source/crh/snowmap.html\" target=\"_blank\">National Weather Service</a>, as of ",current_date_time_pretty,""))


dot_pal <- colorFactor(c("#E0FAFF", "#ADF2FF", "#23DCFF", "#005CFF", "#030D9E", "#1A006B", "#610092"), 
                       domain = c("Less than half an inch", "0.5-1 inches", "1-3 inches", "3-6 inches", "6-9 inches", "9-12 inches", "A foot or more"), 
                       ordered=TRUE)

popup <- paste0("<b style='font-size: 15px'>",snow_clean$location, ", ", snow_clean$state,"</b><br><br> <span style='font-size: 15px'>Snowfall: <b>" , snow_clean$amount, "</b> inches, as of ", snow_clean$valid_time_pretty, ". <br><br> Notes from NWS: " , snow_clean$remarks_new)

wpvi_snow_map <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-75.1892169,39.6227914, zoom = 8) %>%
  addCircleMarkers(data = snow_clean,
                   radius = 8,
                   color = ~dot_pal(color_bin),
                   stroke = FALSE,
                   fillOpacity = 0.8,
                   popup = popup
  ) %>%
  addLegend(values = snow_clean$color_bin,
            position = "bottomleft",
            colors = c("#E0FAFF", "#ADF2FF", "#23DCFF", "#005CFF", "#030D9E", "#1A006B", "#610092"), 
            labels = c("Less than half an inch", "0.5-1 inches", "1-3 inches", "3-6 inches", "6-9 inches", "9-12 inches", "A foot or more"),
            #values = ~amount,
            title = "Snowfall (in inches)") %>%
addControl(title, position = "bottomright", className="map-title")



saveWidget(wpvi_snow_map, 'wpvi_snow_map.html', selfcontained = TRUE)

