# Author: Dominic Contreras
# Assignment: Project 2
# Course: R Shiny for Operations Management
# Date: October 15, 2018

# load required libraries
libraries <- c("shiny", "shinydashboard", "reshape2", "dplyr", "plotly", "shinythemes", "lubridate", 
               "shinyWidgets", "RSocrata", "jsonlite", "ggplot2", "rgdal", "leaflet", "leaflet.extras", 
               "readxl", "stringr", "mapview")
lapply(libraries, require, character.only = TRUE)
remove(libraries)

# read in app token
token <- "4oBQ0Ix5OIq5cxJLpaOpQqxRI"

# pull unique values from 'use of force' data to use as input selectors
# api docs: https://dev.socrata.com/foundry/data.cincinnati-oh.gov/e2va-wsic
dat <- read.socrata("https://data.cincinnati-oh.gov/resource/e2va-wsic.json",
                          app_token = token)
# neighbName <- sort(unique(dat$sna_neighborhood))
# incidentDesc <- sort(unique(dat$incident_description))
# officerGend <- sort(unique(dat$officer_gender))
# officerRace <- sort(unique(dat$officer_race))
# suspectGend <- sort(unique(dat$subject_gender))
# suspectRace <- sort(unique(dat$subject_race))
dat$latitude_x <- as.numeric(dat$latitude_x)
dat$longitude_x <- as.numeric(dat$longitude_x)
dat$officer_gender <- as.factor(dat$officer_gender)

# read in cincinnati sna (statistical neighborhood approximations) boundary shapefile
# api docs: https://data-cagisportal.opendata.arcgis.com/datasets/cincinnati-sna-boundary/geoservice 
cinciNeighb <- readOGR("https://opendata.arcgis.com/datasets/572561553c9e4d618d2d7939c5261d46_0.geojson")

getColor <- function(dat) {
  sapply(dat$officer_gender, function(officer_gender) {
    if(officer_gender == "MALE") {
      "green"
    } else if(officer_gender == "FEMALE") {
      "orange"
    } else {
      "red"
    } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(dat)
)

leaflet() %>% #  Create map
  addProviderTiles("OpenStreetMap.Mapnik", 
                   group = "Street", 
                   options = providerTileOptions(minZoom=12, maxZoom=30)) %>%
  addLayersControl( #  Layer selector (possible to add title to this box?)
    baseGroups = c("Street"), 
    options = layersControlOptions(collapsed = FALSE)) %>%
  addPolygons(data = cinciNeighb, 
              weight = 1.5, 
              color = "black") %>%
  addAwesomeMarkers(data = dat, 
                    lng = ~longitude_x, lat = ~latitude_x,
                    icon = icons,
                    clusterOptions = markerClusterOptions()) %>%
  addMouseCoordinates(style = "basic") %>%
  setView(lng = -84.51, lat = 39.15, zoom = 12) %>%
  setMaxBounds(lng1 = -84.74, lat1 = 39.23, lng2 = -84.34, lat2 = 39.04)

# demographic data
https://insights.cincinnati-oh.gov/stories/s/Census-Data/m8fy-k6n6/




