
# shiny design



#install.packages("ggplot2")
#install.packages("leaflet")
#install.packages("leaflet.extras")
#install.packages("dplyr")
#install.packages("plotly")
#install.packages("shinythemes")
#install.packages("tmap")
#install.packages("shinyWidgets")


#pkg_list = c("ggplot2", "leaflet", "leaflet.extras", "dplyr", "plotly", "shinythemes", "tmap", "stringr", "shinyWidgets", "rbokeh")
#mia_pkgs = pkg_list[!(pkg_list %in% installed.packages()[,"Package"])]
#if(length(mia_pkgs) > 0) install.packages(mia_pkgs)
#deplloaded_pkgs = lapply(pkg_list, require, character.only=TRUE)



#library(tmap)
#library(sf)
require(plotly)
require(dplyr)
require(leaflet.extras)
require(leaflet)
require(ggplot2)
require(shiny)
require(shinydashboard)
require(shinythemes)
require(stringr)
require("rbokeh")
require(shinyWidgets)
library("jsonlite")
# Define UI for application that draws a histogram


# read real time data:


real = fromJSON("https://gbfs.capitalbikeshare.com/gbfs/fr/station_status.json")
real = as.data.frame(real)[,c(-14,-15)]
a = fromJSON("https://gbfs.capitalbikeshare.com/gbfs/fr/station_information.json")$data
a = as.data.frame(a)
colnames(a) = str_extract(colnames(a), '\\b\\w+$')
colnames(real) = str_extract(colnames(real), '\\b\\w+$')
a$station_id = as.character(a$station_id)
real$station_id = as.character(real$station_id)
real_data = na.omit(left_join(real,a,"station_id"))

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  navbarPage(
    # Application title
    title="Bike Sharing in Washington DC",
    tabPanel("About", sidebarPanel(h3("Introduction:"), textOutput("intro")),
             "Designed by: Risberg"),
    # second tab
    tabPanel("Explore all Stations",
             sidebarPanel("Choose Interested Information:",
                          pickerInput("picker", 
                                      label = "Info To Show:", 
                                      choices = c("Station Name"="name", "Available Docks"="num_docks_available", "Disabled Bikes"="num_bikes_disabled", "Station Status"="station_status","Station Capacity"="capacity", "Has Kiosk" = "has_kiosk","Rental Method" = "rental_methods"), 
                                      options = list(`actions-box` = T, size = 15,`selected-text-format` = "count > 3"), 
                                      multiple = TRUE)),
             mainPanel(rbokehOutput("graph",width=800,height=550))),
    
    tabPanel("Predict Bikes", 
             sidebarPanel(
               # Add user inputs here
               
               actionButton(inputId = "action", label = "Predict!")),
             
             
             mainPanel(h3("Predicted Number of Bikes"),
                       verbatimTextOutput("bikes"),
                       h3("Map showing nearest 3 stations"),
                       verbatimTextOutput("Map1"))),
    
    tabPanel("Potential Rush Hours"),
    tabPanel("Heatmap",
             fluidRow(
               column(2,checkboxInput("c1","Bike Heatmap"),
                      checkboxInput("c2","List Top 10 Stations"))),
             leafletOutput("pic1",width="60%",height="750px")
             
    )
    
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # first tab
  output$intro= renderText(
    "Our topic is about: ")
  
  
  # second tab: 
  output$graph=renderRbokeh({
    suppressWarnings(figure(width = 800, height = 450, padding_factor = 0,
                            xlim = range(real_data$lon),ylim = range(real_data$lat)) %>%
                       ly_map("county", "washington",col = 'indigo') %>%
                       ly_points(lon, lat, data =real_data, size = 6,
                                 hover =input$picker)%>%
                       x_axis(visible = T) %>%
                       y_axis(visible = T))
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
