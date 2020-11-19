
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

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  navbarPage(
    # Application title
    title="Bike Sharing in Washington DC",
    tabPanel("About", sidebarPanel(h3("Introduction:"), textOutput("intro")),
             "Designed by: Risberg"),
    
    tabPanel("Explore all Stations"),
    
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

}

# Run the application 
shinyApp(ui = ui, server = server)
