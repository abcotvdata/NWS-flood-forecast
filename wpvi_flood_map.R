library(tidyverse)
library(tidycensus)
library(readr)
library(plyr)
library(stringr)
library(dplyr)
library(leaflet)
library(leaflet.providers)
library(rgdal)
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

c("tidyverse", "tidycensus", "readr", "plyr", "stringr", "dplyr", "leaflet", "leaflet.providers", "rgdal", "htmlwidgets", "RCurl", "sp", "Hmisc", "leafgl", "colourvalues", "sf", "leaflet.extras", "tigris", "reactable", "rmarkdown", "htmltools", "knitr", "RJSONIO", "rjson", "jsonlite", "devtools", "lubridate")


#locations for flood data
locations <- rgdal::readOGR("https://mapservices.weather.noaa.gov/eventdriven/rest/services/water/ahps_riv_gauges/MapServer/15/query?where=0%3D0&objectIds=&time=&resultType=none&outFields=*&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=0&resultRecordCount=&sqlFormat=none&f=pjson")

#filter locations for places that have minor, moderate, major flood warnings
locations_clean <- st_as_sf(locations) %>% 
  filter(status == "minor" | status == "moderate" | status == "major") %>% 
  mutate(status = str_to_title(status))

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
dot_pal <- colorFactor(c("#ffba00", "#df5f00", "#be0000"), domain = c("Minor", "Moderate", "Major"))

popup <- paste0("<b style='font-size: 15px'>",locations_clean$waterbody, " at ", locations_clean$location,"</b><br><br> <span style='font-size: 15px'>Flood status forecast: <b>" , locations_clean$status, "</b></span><br><br><span style='font-size: 15px'>Forecasted peak: <b>" , locations_clean$forecast, " ",locations_clean$units,"</b><br><br><img src=\"https://water.weather.gov/resources/hydrographs/",tolower(locations_clean$gaugelid),"_hg.png\" style=\"width:100%;\"><br><br><a href=\"",locations_clean$url,"\" target=\"_blank\">Click here for more information</a></span>")

flood_map <-  leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-75.0577017, 40.0639383, zoom = 6) %>% 
  addCircleMarkers(data = locations_clean, ~longitude, ~latitude,
                   radius = 6,
                   color = ~dot_pal(status),
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   popup = popup
  ) %>% 
  addLegend(pal = dot_pal, 
            values = locations_clean$status, 
            position = "bottomleft", 
            title = "Flood forecast levels") %>% 
  addControl(title, position = "bottomright", className="map-title")


saveWidget(flood_map, 'wpvi_flood_map.html', selfcontained = TRUE)
