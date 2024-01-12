library(tidyverse)
library(tidycensus)
library(readr)
library(plyr)
library(stringr)
library(dplyr)
library(leaflet)
library(leaflet.providers)
library(htmlwidgets)
library(RCurl)
library(sp)
library(Hmisc)
library(stringr)
library(leafgl)
library(colourvalues)
library(sf)
library(leaflet.extras)
library(tigris)
library(reactable)
library(rmarkdown)
library(htmltools)
library(knitr)
library(RJSONIO)
library(rjson)
library(jsonlite)
library(devtools)
library(lubridate)
library(rvest)

#install.packages("gdal")

#c("tidyverse", "tidycensus", "readr", "plyr", "stringr", "dplyr", "leaflet", "leaflet.providers", "rgdal", "htmlwidgets", "RCurl", "sp", "Hmisc", "leafgl", "colourvalues", "sf", "leaflet.extras", "tigris", "reactable", "rmarkdown", "htmltools", "knitr", "RJSONIO", "rjson", "jsonlite", "devtools", "lubridate")


#locations for flood data
locations <- st_read("https://mapservices.weather.noaa.gov/eventdriven/rest/services/water/ahps_riv_gauges/MapServer/15/query?where=0%3D0&objectIds=&time=&resultType=none&outFields=*&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=0&resultRecordCount=&sqlFormat=none&f=pjson")

#filter locations for places that have minor, moderate, major flood warnings
locations_clean <- st_as_sf(locations) %>% 
  filter(status == "minor" | status == "moderate" | status == "major") %>% 
  mutate(status = str_to_title(status))

#https://water.weather.gov/ahps2/hydrograph_to_xml.php?gage=phbp1&output=tabular
gages <- locations_clean$gaugelid
forecast_peaks <- data.frame()

for (gage in gages) {
  url = paste(sep="","https://water.weather.gov/ahps2/hydrograph_to_xml.php?gage=", gage ,"&output=tabular")
  print(url)
  
  webpage <- read_html(url)
  forecast_table_list <- webpage %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
  table_list_length <- as.numeric(length(forecast_table_list))
  forecast_table_list_last <- webpage %>%
    html_nodes("table") %>%
    .[table_list_length] %>%
    html_table(fill = TRUE)
  forecast_table_df <- forecast_table_list_last[[1]]
  colnames(forecast_table_df) <- c("Date(UTC)", "Stage")
  forecast_table_df_clean <- forecast_table_df %>% 
    select(`Date(UTC)`, Stage) %>% 
    slice(-1, -2) %>% 
    mutate(Stage = str_replace(Stage, "ft", "")) %>% 
    mutate(Stage = as.numeric(Stage)) %>% 
    mutate(Date = substr(`Date(UTC)`, 1,5)) %>% 
    mutate(Date = paste(sep="", Date, "/2024")) %>% 
    mutate(Time = substr(`Date(UTC)`, 7,11)) %>% 
    mutate(DateTime = paste(sep="", Date, " ", Time)) %>% 
    mutate(DateTime_Clean = as.POSIXct(DateTime, format="%m/%d/%Y %H:%M", tz="UTC")) %>% 
    mutate(DateTime_EST = with_tz(DateTime_Clean, "EST"))
  
  max_forecast <- max(forecast_table_df_clean$Stage)
  max_forecast_time_filter <- forecast_table_df_clean %>% 
    filter(Stage == max_forecast)
  max_forecast_time <- as.character(format(max_forecast_time_filter$DateTime_EST, '%B %d, %Y - %I:%M %p %Z'))
  
  current_gage_peak <- data.frame (gaugelid  = gage,
              max_forecast = max_forecast,
              max_forecast_time = max_forecast_time)
  
  forecast_peaks <- bind_rows(forecast_peaks, current_gage_peak)
  
}

forecast_peaks <- forecast_peaks %>% 
  distinct(gaugelid, .keep_all = TRUE)

locations_clean <- merge(locations_clean, forecast_peaks, by="gaugelid")


#get date
timestamp_injest <- locations_clean$idp_ingestdate[1]
timestamp_injest_new <- substr(timestamp_injest, 1, 10)
timestamp_injest_clean <- as.POSIXct(as.numeric(timestamp_injest_new), origin='1970-01-01', tz="EST") #should probably make this a better date
timestamp_injest_pretty <- as.character(format(timestamp_injest_clean, '%B %d, %Y - %I:%M %p %Z'))


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
  tag.map.title, HTML("Source: <a href=\"https://www.weather.gov/gis/cloudgiswebservices\">National Weather Service</a>, as of ",timestamp_injest_pretty,""))

poly_pal <- colorFactor(c("#ffba00", "#df5f00", "#be0000"), domain = c("Possible", "Likely", "Occuring"))
dot_pal <- colorFactor(c("#ffba00", "#df5f00", "#be0000"), domain = c("Minor", "Moderate", "Major"), ordered=TRUE)

popup <- paste0("<b style='font-size: 15px'>",locations_clean$waterbody, " at ", locations_clean$location,"</b><br><br> <span style='font-size: 15px'>Flood status forecast: <b>" , locations_clean$status, "</b></span><br><br><span style='font-size: 15px'>Forecasted peak: <b>" , locations_clean$forecast, " ",locations_clean$units," on ", locations_clean$max_forecast_time, "</b><br><br><img src=\"https://water.weather.gov/resources/hydrographs/",tolower(locations_clean$gaugelid),"_hg.png\" style=\"width:100%;\"><br><br><a href=\"",locations_clean$url,"\" target=\"_blank\">Click here for more information</a></span>")

flood_map <-  leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-75.1892169,39.6227914, zoom = 8) %>% 
  addCircleMarkers(data = locations_clean, ~longitude, ~latitude,
                   radius = 8,
                   color = ~dot_pal(status),
                   stroke = FALSE,
                   fillOpacity = 0.8,
                   popup = popup
  ) %>% 
  addLegend(values = locations_clean$status, 
            position = "bottomleft", 
            colors = c("#ffba00", "#df5f00", "#be0000"), 
            labels = c("Minor", "Moderate", "Major"),
            title = "Flood forecast levels") %>% 
  addControl(title, position = "bottomright", className="map-title")


saveWidget(flood_map, 'wpvi_flood_map.html', selfcontained = TRUE)
