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
neighbName <- sort(unique(dat$sna_neighborhood))
incidentDesc <- sort(unique(dat$incident_description))
officerGend <- sort(unique(dat$officer_gender))
officerRace <- sort(unique(dat$officer_race))
suspectGend <- sort(unique(dat$subject_gender))
suspectRace <- sort(unique(dat$subject_race))

# read in cincinnati sna (statistical neighborhood approximations) boundary shapefile
# api docs: https://data-cagisportal.opendata.arcgis.com/datasets/cincinnati-sna-boundary/geoservice 
cinciNeighb <- readOGR("https://opendata.arcgis.com/datasets/572561553c9e4d618d2d7939c5261d46_0.geojson")

# read in demoraphic information (generated data, need to sub-out for regular data)
demoData <- read.csv("http://www.sharecsv.com/dl/b4d4d59571b412a2d5025f38ffa3a336/testData.csv")

# title + data source notification
header <- dashboardHeader(title = "Cincinnati Police Use of Force Data",
                          dropdownMenu(type = "notifications",
                                       notificationItem(text = "Source: Chicago Data Portal", 
                                                        icon = icon("fa fa-exclamation-triangle"))
                          )
)

# side bar layout 
sidebar <- dashboardSidebar(
  sidebarMenu( # toggle between pages
    id = "tabs",
    menuItem("Map", icon = icon("bar-chart"), tabName = "map"),
    menuItem("Charts", icon = icon("location-arrow"), tabName = "graphs"),
    menuItem("Download Data", icon = icon("download"), tabName = "table"),
    
    # Neighborhood
    selectizeInput("neighbSelect", 
                   "Neighborhoods:", 
                   choices = neighbName, 
                   multiple = TRUE,
                   options = list(placeholder = 'Select neighborhood(s)')),
    
    # Incident Description
    selectizeInput("incSelect", 
                   "Incident Type:", 
                   choices = incidentDesc, 
                   multiple = TRUE,
                   options = list(placeholder = 'Select incident type(s)')),
    
    # Officer Gender
    selectizeInput("offGendSelect", 
                   "Officer Gender:", 
                   choices = c(officerGend, "ALL"), 
                   multiple = FALSE,
                   selected = "ALL"),
    
    # Officer Race
    selectizeInput("offRaceSelect", 
                   "Officer Race:", 
                   choices = c(officerRace, "ALL"), 
                   multiple = FALSE,
                   selected = "ALL"),
    
    # Suspect Gender
    selectizeInput("susGendSelect", 
                   "Suspect Gender:", 
                   choices = c(suspectGend, "ALL"), 
                   multiple = FALSE,
                   selected = "ALL"),
    
    
    # Suspect Race
    selectizeInput("susRaceSelect", 
                   "Suspect Race:", 
                   choices = c(suspectRace, "ALL"), 
                   multiple = FALSE,
                   selected = "ALL"),
    
    # Date range
    dateRangeInput("dateSelect",
                   "Date Range:", 
                   start = "1996-07-01", end = Sys.Date()-7, 
                   min = "1996-07-01", max = Sys.Date()-7, 
                   format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                   language = "en", separator = " to ", width = NULL),
    
    # Action button to reset filters, keeping original icon b/c works well
    actionButton("reset", "Reset Filters", icon = icon("refresh")) 
  )
)

# tab layout for plots
body <- dashboardBody(tabItems(
  
  # Create page 1 (map of use of force incidents)
  tabItem("map",
          
          # Name tabs
          fluidRow(
            valueBoxOutput("totalCrimes"),
            valueBoxOutput("pctSolved"),
            valueBoxOutput("mostCommon")
          ),
          fluidRow(
            tabBox(width = 12, height = 200,
                   
                   # Layout and description of tab 1
                   tabPanel("Map", 
                            HTML("<p><em>The graph below shows the frequency of a reported crime for the timeframe selected.&nbsp;</em></p>"),
                            leafletOutput("plot_map")))
          )
  ),
  
  # Create page 2 (graphs displaying use of force data)
  tabItem("graphs",
          fluidRow(
            tabBox(width = 12,
                   
                   # Layout and description of tab 1
                   tabPanel("Descriptor title 1",
                            HTML("<p><em>The graph below shows the 10 most frequent locations of the crime(s) selected for the time period selected.&nbsp;</em></p>"),
                            plotlyOutput("plot_graph1"),
                            radioButtons("fillSelect", 
                                         "Select Fill:", 
                                         choices = c("officer_race", "officer_gender", "subject_race", "subject_gender"), 
                                         selected = "officer_race", 
                                         inline = TRUE,
                                         width = NULL)),
                   
                   # Layout and description of tab 2
                   tabPanel("Descriptor title 2",
                            HTML("<p><em>The graph below shows the 10 most frequent locations of the crime(s) selected for the time period selected.&nbsp;</em></p>"),
                            plotlyOutput("plot_graph2")))
          )
  ),
  
  # Create page 3 (table)
  tabItem("table",
          inputPanel(
            downloadButton("downloadData","Download Use of Force Data") # add button to download table as csv
          ),
          fluidPage(
            box(title = "Selected Crime Stats", DT::dataTableOutput("table"), width = 24))
  )
)
)

ui <- dashboardPage(header, sidebar, body)

# Define Server Logic
server <- function(input, output, session = session) {
  forceInput <- reactive({
    
    # no neighbor & no type
    if (length(input$neighbSelect) == 0 & length(input$incSelect) == 0) {
      force <- read.socrata(paste0("https://data.cincinnati-oh.gov/resource/e2va-wsic.json?$where=incident_date >= '", 
                                   input$dateSelect[1], "T00:00:00' AND incident_date <= '", 
                                   input$dateSelect[2], "T23:59:59'"), 
                            app_token = token)
      
      # no neighbor & one type
    } else if (length(input$neighbSelect) == 0 & length(input$incSelect) == 1) {
      force <- read.socrata(paste0("https://data.cincinnati-oh.gov/resource/e2va-wsic.json?$where=incident_date >= '", 
                                   input$dateSelect[1], "T00:00:00' AND incident_date <= '", 
                                   input$dateSelect[2], "T23:59:59' AND incident_description= '", 
                                   input$incSelect[1], "'"), 
                            app_token = token)
      
      # no neighbor & multiple types
    } else if (length(input$neighbSelect) == 0 & length(input$incSelect) > 1) {
      incident_collapse <- paste0(input$incSelect, collapse = "' OR incident_description= '")
      force <- read.socrata(paste0("https://data.cincinnati-oh.gov/resource/e2va-wsic.json?$where=incident_date >= '", 
                                   input$dateSelect[1], "T00:00:00' AND incident_date <= '", 
                                   input$dateSelect[2], "T23:59:59' AND (incident_description= '", 
                                   incident_collapse, "')"), 
                            app_token = token)
      
      # one neighbor & no type
    } else if (length(input$neighbSelect) == 1 & length(input$incSelect) == 0) {
      force <- read.socrata(paste0("https://data.cincinnati-oh.gov/resource/e2va-wsic.json?$where=incident_date >= '", 
                                   input$dateSelect[1], "T00:00:00' AND incident_date <= '", 
                                   input$dateSelect[2], "T23:59:59' AND sna_neighborhood= '", 
                                   input$neighbSelect[1], "'"), 
                            app_token = token)
      
      # one neighbor & one type
    } else if (length(input$neighbSelect) == 1 & length(input$incSelect) == 1) {
      force <- read.socrata(paste0("https://data.cincinnati-oh.gov/resource/e2va-wsic.json?$where=incident_date >= '", 
                                   input$dateSelect[1], "T00:00:00' AND incident_date <= '", 
                                   input$dateSelect[2], "T23:59:59' AND incident_description= '", 
                                   input$incSelect[1], "' AND sna_neighborhood= '", 
                                   input$neighbSelect[1], "'"), 
                            app_token = token)
      
      # one neighbor & multiple types
    } else if (length(input$neighbSelect) == 1 & length(input$incSelect) > 1) {
      incident_collapse <- paste0(input$incSelect, collapse = "' OR incident_description= '")
      force <- read.socrata(paste0("https://data.cincinnati-oh.gov/resource/e2va-wsic.json?$where=incident_date >= '", 
                                   input$dateSelect[1], "T00:00:00' AND incident_date <= '", 
                                   input$dateSelect[2], "T23:59:59' AND (incident_description= '", 
                                   incident_collapse, "') AND sna_neighborhood= '", 
                                   input$neighbSelect[1], "'"), 
                            app_token = token)
      
      # multiple neighbor & no type
    } else if (length(input$neighbSelect) > 1 & length(input$incSelect) == 0) {
      neighbor_collapse <- paste0(input$neighbSelect, collapse = "' OR sna_neighborhood= '")
      force <- read.socrata(paste0("https://data.cincinnati-oh.gov/resource/e2va-wsic.json?$where=incident_date >= '", 
                                   input$dateSelect[1], "T00:00:00' AND incident_date <= '", 
                                   input$dateSelect[2], "T23:59:59' AND (sna_neighborhood= '", 
                                   neighbor_collapse, "')"), 
                            app_token = token)
      
      # multiple neighbor & one type
    } else if (length(input$neighbSelect) > 1 & length(input$incSelect) == 1) {
      neighbor_collapse <- paste0(input$neighbSelect, collapse = "' OR sna_neighborhood= '")
      force <- read.socrata(paste0("https://data.cincinnati-oh.gov/resource/e2va-wsic.json?$where=incident_date >= '", 
                                   input$dateSelect[1], "T00:00:00' AND incident_date <= '", 
                                   input$dateSelect[2], "T23:59:59' AND incident_description= '", 
                                   input$incSelect[1], "' AND (sna_neighborhood= '", 
                                   neighbor_collapse, "')"), 
                            app_token = token)
      
      # multiple neighbor & multiple types
    } else {
      incident_collapse <- paste0(input$incSelect, collapse = "' OR incident_description= '")
      neighbor_collapse <- paste0(input$neighbSelect, collapse = "' OR sna_neighborhood= '")
      force <- read.socrata(paste0("https://data.cincinnati-oh.gov/resource/e2va-wsic.json?$where=incident_date >= '", 
                                   input$dateSelect[1], "T00:00:00' AND incident_date <= '", 
                                   input$dateSelect[2], "T23:59:59' AND (incident_description= '", 
                                   incident_collapse, "') AND (sna_neighborhood= '", 
                                   neighbor_collapse, "')"), 
                            app_token = token)
    }
    
    # keep only relevant columns
    force <- select(force, incident_date, incident_description, 
                    latitude_x, longitude_x, officer_gender, officer_race, 
                    sna_neighborhood, subject_gender, subject_race)
    
    # get rid of NAs
    force <- na.omit(force)
    
    # subset by officer gender
    if (input$offGendSelect != "ALL") {
      force <- subset(force, officer_gender %in% input$offGendSelect)
    }
    
    # subset by officer race
    if (input$offRaceSelect != "ALL") {
      force <- subset(force, officer_race %in% input$offRaceSelect)
    }
    
    # subset by subject gender
    if (input$susGendSelect != "ALL") {
      force <- subset(force, subject_gender %in% input$susGendSelect)
    }
    
    # subset by subject race
    if (input$susRaceSelect != "ALL") {
      force <- subset(force, subject_race %in% input$susRaceSelect)
    }
    return(force)
  })
  
  # map
  output$plot_map <- renderLeaflet ({
    leaflet() %>% # Create map
      addProviderTiles("OpenStreetMap.Mapnik", 
                       group = "Street", 
                       options = providerTileOptions(minZoom=12, maxZoom=30)) %>%
      addLayersControl( # Layer selector (possible to add title to this box?)
        baseGroups = c("Street"), 
        options = layersControlOptions(collapsed = FALSE)) %>%
      addPolygons(data = cinciNeighb, 
                  weight = 1.5, 
                  color = "black") %>%
      addAwesomeMarkers(data = forceInput(), 
                        lng = ~as.numeric(longitude_x), lat = ~as.numeric(latitude_x),
                        clusterOptions = markerClusterOptions()) %>%
      addMouseCoordinates(style = "basic") %>%
      setView(lng = -84.51, lat = 39.15, zoom = 12) %>%
      setMaxBounds(lng1 = -84.74, lat1 = 39.23, lng2 = -84.34, lat2 = 39.04)
  })
  
  # frequency plot
  output$plot_graph1 <- renderPlotly({
    ggplotly(
      ggplot(data = forceInput(), aes(x = sna_neighborhood,
                                      text = paste0("<b>Total Crimes: ", ..count.., "</b>"))) +
        aes_string(fill = input$fillSelect) +
        geom_histogram(stat = "count") +
        labs(y = "Count",
             title = "Number of Reports by Crime Type",
             x = NULL) +
        theme(plot.title = element_text(family = 'Helvetica',  
                                        color = '#181414', 
                                        face = 'bold', 
                                        size = 18, 
                                        hjust = 0)) +
        theme(axis.title.y = element_text(family = 'Helvetica', 
                                          color = '#181414', 
                                          face = 'bold', 
                                          size = 12, 
                                          hjust = 0)) +
        theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) + 
        guides(color = FALSE)
      , tooltip = "text")
  })
  
  # Downloadable crime datatable
  output$table <- DT::renderDataTable({
    subset(forceInput(), select = c(sna_neighborhood, incident_description))
  },
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '200px', targets = "_all"))
  ))
}

# Run the application 
shinyApp(ui = ui, server = server)

## Later!!!

#getColor <- function(dat) {
# sapply(dat$officer_gender, function(officer_gender) {
#  if(officer_gender == "MALE") {
#   "green"
#} else if(officer_gender == "FEMALE") {
# "orange"
#} else {
#  "red"
#} })
#}

#icons <- awesomeIcons(
#  icon = 'ios-close',
# iconColor = 'black',
#library = 'ion',
#markerColor = getColor(dat)
#)

#leaflet() %>% #  Create map
# addProviderTiles("OpenStreetMap.Mapnik", 
#                 group = "Street", 
#                options = providerTileOptions(minZoom=12, maxZoom=30)) %>%
#addLayersControl( #  Layer selector (possible to add title to this box?)
# baseGroups = c("Street"), 
#  options = layersControlOptions(collapsed = FALSE)) %>%
# addPolygons(data = cinciNeighb, 
#          weight = 1.5, 
#         color = "black") %>%
#  addAwesomeMarkers(data = dat, 
#                   lng = ~longitude_x, lat = ~latitude_x,
#                  icon = icons,
#                 clusterOptions = markerClusterOptions()) %>%
# addMouseCoordinates(style = "basic") %>%
# setView(lng = -84.51, lat = 39.15, zoom = 12) %>%
# setMaxBounds(lng1 = -84.74, lat1 = 39.23, lng2 = -84.34, lat2 = 39.04)



