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
    menuItem("Map", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Charts", icon = icon("location-arrow"), tabName = "loc"),
    menuItem("Download Data", icon = icon("download"), tabName = "table"),
    
    # Neighborhood
    selectizeInput("neighbSelect", 
                   "Neighborhoods:", 
                   choices = neighbName, 
                   multiple = TRUE,
                   options = list(placeholder = 'Select neighborhood(s)')),
    
    # Officer Gender
    radioButtons("offGendSelect", 
                 "Officer Gender", 
                 choices = officerGend, 
                 selected = officerGend[1]),
    
    # Suspect Gender
    radioButtons("susGendSelect", 
                 "Suspect Gender", 
                 choices = suspectGend, 
                 selected = suspectGend[1]),
    
    # Officer Race
    selectizeInput("offRaceSelect", 
                   "Officer Race", 
                   choices = officerRace, 
                   multiple = FALSE,
                   selected = officerRace[1]),
    
    # Suspect Race
    selectizeInput("susRaceSelect", 
                   "Suspect Race", 
                   choices = officerRace, 
                   multiple = FALSE,
                   selected = suspectRace[1]),
    
    # Time of day
    radioButtons("timeSelect", 
                 "Time of Day:",
                 choices = c("morning", "afternoon", "evening", "night", "all"),
                 selected = "all"),
    
    # Date range
    dateRangeInput("dateSelect",
                   "Date Range:", 
                   start = Sys.Date()-38, end = Sys.Date()-7, 
                   min = "2001-01-01", max = Sys.Date()-7, 
                   format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                   language = "en", separator = " to ", width = NULL),
    
    # Action button to reset filters, keeping original icon b/c works well
    actionButton("reset", "Reset Filters", icon = icon("refresh")) 
  )
)

ui <- dashboardPage(header, sidebar, body)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)