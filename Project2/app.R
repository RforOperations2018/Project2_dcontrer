# Author: Dominic Contreras
# Assignment: Project 2
# Course: R Shiny for Operations Management
# Date: October 15, 2018

# load required libraries
libraries <- c("shiny", "shinydashboard", "reshape2", "dplyr", "plotly", "shinythemes", "lubridate", 
               "shinyWidgets", "RSocrata", "jsonlite", "ggplot2", "rgdal", "leaflet", "leaflet.extras", 
               "readxl", "stringr", "mapview", "formattable", "scales")
lapply(libraries, require, character.only = TRUE)
remove(libraries)

# read in app token
token <- "4oBQ0Ix5OIq5cxJLpaOpQqxRI"

# pull unique values from 'use of force' data to use as input selectors
dat <- read.socrata("https://data.cincinnati-oh.gov/resource/e2va-wsic.json",
                    app_token = token)
neighbName <- sort(unique(dat$sna_neighborhood))
incidentDesc <- sort(unique(dat$incident_description))
officerGend <- sort(unique(dat$officer_gender))
officerRace <- sort(unique(dat$officer_race))
suspectGend <- sort(unique(dat$subject_gender))
suspectRace <- sort(unique(dat$subject_race))
dateDefault <- range(dat$incident_date, na.rm = T)
dateMin <- substr(dateDefault[1], start = 1, stop = 10)
dateMax <- substr(dateDefault[2], start = 1, stop = 10)
remove(dateDefault)
remove(dat)

# read in cincinnati neighborhood boundary shapefile
cinciNeighb <- readOGR("https://opendata.arcgis.com/datasets/572561553c9e4d618d2d7939c5261d46_0.geojson")

# read in cincinnati demographic information
demoDat <- read.csv("http://www.sharecsv.com/dl/20b0ce686f4ede9d1e3e9f56e12e400c/cincIncome.csv")

# format title and data source notification
header <- dashboardHeader(title = "Cincinnati Police Data",
                          dropdownMenu(type = "notifications",
                                       notificationItem(text = "Source: Open Data Cincinnati", 
                                                        icon = icon("fa fa-exclamation-triangle"))
                          )
)

# side bar layout 
sidebar <- dashboardSidebar(
  sidebarMenu( # page switch between data visualizations and data download
    id = "tabs",
    menuItem("See Data", icon = icon("eye"), tabName = "map"),
    menuItem("Download Data", icon = icon("download"), tabName = "table"),
    
    # neighborhood selector
    selectizeInput("neighbSelect", 
                   "Neighborhoods:", 
                   choices = neighbName, 
                   multiple = TRUE,
                   options = list(placeholder = 'Select neighborhood(s)')),
    
    # incident description selector
    selectizeInput("incSelect", 
                   "Incident Type:", 
                   choices = incidentDesc, 
                   multiple = TRUE,
                   options = list(placeholder = 'Select incident type(s)')),
    
    # officer gender selector
    selectizeInput("offGendSelect", 
                   "Officer Gender:", 
                   choices = c(officerGend, "ALL"), 
                   multiple = FALSE,
                   selected = "ALL"),
    
    # officer race selector
    selectizeInput("offRaceSelect", 
                   "Officer Race:", 
                   choices = c(officerRace, "ALL"), 
                   multiple = FALSE,
                   selected = "ALL"),
    
    # suspect gender selector
    selectizeInput("susGendSelect", 
                   "Suspect Gender:", 
                   choices = c(suspectGend, "ALL"), 
                   multiple = FALSE,
                   selected = "ALL"),
    
    
    # suspect race selector
    selectizeInput("susRaceSelect", 
                   "Suspect Race:", 
                   choices = c(suspectRace, "ALL"), 
                   multiple = FALSE,
                   selected = "ALL"),
    
    # date range selector
    dateRangeInput("dateSelect",
                   "Date Range:", 
                   start = Sys.Date()-365, end = dateMax, 
                   min = dateMin, max = dateMax, 
                   format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                   language = "en", separator = " to ", width = NULL),
    
    # reset selector
    actionButton("reset", "Reset Filters", icon = icon("refresh")) 
  )
)

# tab layout for plots
body <- dashboardBody(tabItems(
  
  # create viz pages 
  tabItem("map",
          fluidRow(
            tabBox(width = 12, height = 200,
                   
                   # layout for viz 1 - map 
                   tabPanel("Where are police using force?", 
                            HTML("<p><em>The map below shows locations where police officers used force based on the parameters selected.&nbsp;</em></p>"),
                            leafletOutput("plot_map")),
                   
                   # layout for viz 2 - barchart
                   tabPanel("Who are police using force against?",
                            HTML("<p><em>The graph below shows demographic information about officers and subjects, based on the parameters selected.&nbsp;</em></p>"),
                            plotlyOutput("plot_graph1", height = "460px"),
                            radioButtons("fillSelect", 
                                         "What would you like bars to be segmented by?", 
                                         choices = c("officer_race", "officer_gender", "subject_race", "subject_gender"), 
                                         selected = "officer_race", 
                                         inline = TRUE,
                                         width = NULL)),
                   
                   # layout for viz 3 - scatterplot
                   tabPanel("Is there a connection between use of force and income?",
                            HTML("<p><em>The scatterplot below shows the relationship between total incidents in a neighborhood and median household income for that neighborhood, based on the parameters selected.&nbsp;</em></p>"),
                            plotlyOutput("plot_graph2")))
          )
  ),
  
  # create data table layer 
  tabItem("table",
          inputPanel(
            
            # add button to download table as csv
            downloadButton("downloadData","Download Use of Force Data") 
          ),
          fluidPage(
            box(title = "Selected Crime Stats", DT::dataTableOutput("table"), width = 24))
  )
))


ui <- dashboardPage(header, sidebar, body)

# define server logic
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
  
  # plot map
  output$plot_map <- renderLeaflet ({
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik", 
                       group = "Street", 
                       options = providerTileOptions(minZoom=11, maxZoom=30)) %>%
      addPolygons(data = cinciNeighb, 
                  weight = 1.5, 
                  color = "black") %>%
      addAwesomeMarkers(data = forceInput(), 
                        lng = ~as.numeric(longitude_x), 
                        lat = ~as.numeric(latitude_x),
                        label = ~incident_date,
                        clusterOptions = markerClusterOptions()) %>%
      addMouseCoordinates(style = "basic") %>%
      setView(lng = -84.51, lat = 39.15, zoom = 11) %>%
      setMaxBounds(lng1 = -84.74, lat1 = 39.23, lng2 = -84.34, lat2 = 39.04)
  })
  
  # plot histogram
  output$plot_graph1 <- renderPlotly({
    ggplotly(
      ggplot(data = forceInput(), aes(x = sna_neighborhood,
                                      text = paste0("<b>Total: ", comma(..count.., digits = 0L), "</b>"))) +
        aes_string(fill = input$fillSelect) +
        geom_histogram(stat = "count") +
        labs(y = "Total Number of Incidents",
             title = "Police Use of Force by Neighborhood & Demographics Characteristics",
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
        scale_y_continuous(labels = comma) +
        guides(color = FALSE)
      , tooltip = "text")
  })
  
  # plot scatterplot
  output$plot_graph2 <- renderPlotly({
    scatterInput <- reactive({
      forceInput() %>%
        count(sna_neighborhood) %>%
        merge(demoDat, by.x = "sna_neighborhood", by.y = "neighb")
    })
    dat <- scatterInput()
    ggplotly(
      ggplot(data = dat, aes(x = as.numeric(medIncome), y = as.numeric(n),
                             text = paste0("<b>Neighborhood: </b>", sna_neighborhood, "<br>",
                                           "<b>Total Uses of Force: </b>", comma(as.numeric(n), digits = 0L), "<br>",
                                           "<b>Median Household Income: </b>", currency(medIncome, digits = 0L), "</b>"))) + 
        geom_point() + geom_smooth(method = "lm") + 
        labs(title = "Relationship Between Use of Force and Median Household Income",
             y = "Total Uses of Force",
             x = "Median Household Income") +
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
        scale_x_continuous(labels = dollar) +
        scale_y_continuous(labels = comma) +
        guides(color = FALSE)
      , tooltip = "text")
  })
  
  # render crime datatable
  output$table <- DT::renderDataTable({
    subset(forceInput(), select = colnames(forceInput()))
  },
  options = list(
    autoWidth = TRUE,
    scrollX = TRUE,
    columnDefs = list(list(width = '200px', targets = "_all"))
  ))
  
  # url bar update
  observe({
    print(reactiveValuesToList(input))
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
  # make data table downloadable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("cincinnati-force-data", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(forceInput(), file)
    }
  )
  
  # reset filters
  observeEvent(input$reset, {
    updateSelectInput(session, "neighbSelect", selected = c(""))
    updateSelectInput(session, "incSelect", selected = c(""))
    updateSelectInput(session, "offGendSelect", selected = "ALL")
    updateSelectInput(session, "offRaceSelect", selected = "ALL")
    updateSelectInput(session, "susGendSelect", selected = "ALL")
    updateSelectInput(session, "susRaceSelect", selected = "ALL")
    updateDateRangeInput(session, "dateSelect", start = Sys.Date()-365, end = dateMax)
    showNotification("You have reset the filters", 
                     type = "message", 
                     duration = 3, 
                     closeButton = F)
  })
}

# run the application 
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



