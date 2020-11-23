
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

full_20 = read.csv("./data/bikes_2020_full.csv") %>% dplyr::filter(Month<=10) %>% na.omit()
#new_real = real_data
#colnames(new_real)[22] = "End.station"
#new_full$End.station = as.factor(new_full$End.station)
#new_real$End.station = as.factor(new_real$End.station)
#new_full = left_join(full_20,new_real[,c(22,27)], "End.station")
#new_full$capacity = as.numeric(new_full$capacity)


#new_full[new_full$Start.station == i|new_full$End.station == i,]$flow = with(new_full%>%filter(Start.station == i |End.station == i),ifelse(Start.station == i,-1, 1))

  
  
full_19 = read.csv("./data/bike_2019_full.csv") %>% na.omit()
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  navbarPage(
    # Application title
    title="Bike Sharing in Washington DC",
    tabPanel("About", sidebarPanel(
      
      h3("Welcome to Bike Sharing"), htmlOutput("intro")),
             "Designed by: Risberg(Yixuan Luo, Zixia Luan, Steve Kim, Jie Luo)",
             mainPanel(img(src='Bike1.png', width=800, height=550))),
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
               selectInput("sta", "Choose a Station:",
                           choices = levels(full_20$Start.station),selected = "Jefferson Dr & 14th St SW"),
               selectInput("mon", "Choose a Month:",
                           #choices = levels(as.factor(full_20$Month)),selected = "1"),
                           choices = c("1","2","3","4","5","6","7","8","9","10","11","12"),selected = "1"),
               selectInput("day", "Choose a Day:",
                           choices = levels(as.factor(full_20$Day)),selected = "1"),
               selectInput("hour", "Choose an Hour:",
                           choices = levels(as.factor(full_20$Hour)),selected = "1"),
               selectInput("week", "Is it a Weekend?",
                           choices = c("Yes", "No")),
               selectInput("holi", "Is it a Holiday?",
                           choices = c("Yes", "No")),

               actionButton(inputId = "action", label = "Predict!")),
             
             mainPanel(h3("Predicted Number of Bikes"),
                       verbatimTextOutput("bikes"),
                       h3("Map showing nearest 3 stations"),
                       verbatimTextOutput("Map1"))),
    
    tabPanel("Potential Rush Hours",
             sidebarPanel("Popular Times",
                          
                          selectInput("dataset", "Choose a Period:",
                                      choices = c("Covid Days", "Usual Days")),
                          
                          actionButton("update", "Update Period"),
                          
                          helpText("Note: click to view the updates."),
                          selectInput("Station", "Choose a Station:",
                                      choices = levels(full_20$Start.station)),
                          
                          selectInput("Month", "Choose a Month:",
                                      choices = levels(as.factor(full_20$Month)),selected = "1"),
                                        
                          
                          selectInput("Weekend", "Is it a Weekend?",
                                      choices = c("Yes", "No"))
             ),
             mainPanel(plotOutput("graph4")))

  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # first tab
  output$intro= renderText({
    mylist <- c("<B>This app helps you check the information for each bike station</B>", 
                " ",
                "<B>How it works:</B>", 
                " ",
                "<B>Explore all stations</B>: Interactive map showing selective information from real-time data (Station name, available docks, etc).",
                " ",
                "<B>Predict Bikes</B>: The number of available bikes in station chosen by users using our predicting models.",
                " ",
                "<B>Potential rush hours</B>: Popular time during the day in station selected by users")
    paste(mylist, collapse = "<br>")
  })

  
  # second tab: 
  output$graph=renderRbokeh({
    suppressWarnings(figure(width = 800, height = 450, padding_factor = 0,
                            xlim = range(real_data$lon),ylim = range(real_data$lat)) %>%
                       ly_map("state",col = 'gray') %>%
                       ly_points(lon, lat, data =real_data, size = 6,
                                 hover =input$picker)%>%
                       x_axis(visible = T) %>%
                       y_axis(visible = T))
  })
  
 # third tab
  try_data <- reactive({
    filter(full_20, Start.station == input$sta |End.station == input$sta)
    })
  
  initial <- reactive({real_data %>%
    filter(name == input$sta) %>%
    select(capacity) %>%
    as.numeric()  })
  try_data_1 =  reactive({    
    try_data ()%>%
      mutate(flow = ifelse(Start.station == input$sta,-1, 1))%>%
      mutate(Weekend = ifelse(Weekend == 1, "Yes", "No")) %>%
      mutate(Holiday = ifelse(Holiday == 1, "Yes", "No")) %>%
      #filter(Start.station == input$sta | End.station == input$sta) %>%
      group_by(Month, Day, Hour) %>%
      mutate(n = sum(flow)) %>%
      select(Month, Day, Hour, Weekend, Holiday, n) %>%
      distinct()%>%
      group_by(Month, Day) %>%
      mutate(n = ifelse(row_number()==1, n + 0.4*initial(), n)) %>%
      mutate(cumsum = cumsum(n)) %>%
      na.omit()
    })
  
  model =reactive({lm(cumsum ~ as.factor(Hour) + Weekend + Holiday + as.factor(Month) + Day, data =  try_data_1())})
  
  new_data = reactive({data.frame(Hour=input$hour, Weekend=input$week, Holiday = input$holi,Month=as.character(ifelse(as.numeric(input$mon)>=10,"10",input$mon)), Day=as.numeric(input$day))})
  
  predictVal = eventReactive(input$action,{
    predict(model(), new_data())
  })
  
  output$bikes = renderPrint(
    {
      #print(predictVal())
      max(0,predictVal())
    })
  

  
  
  # fourth tab  
  datasetInput <- eventReactive(input$update, {
    switch(input$dataset,
           "Usual Days" = full_19,
           "Covid Days" = full_20
    )
  }, ignoreNULL = FALSE)
  
  peakfinder <- function(d){
    dh <- hist(d,plot=FALSE)
    ins <- dh[["intensities"]]
    nbins <- length(ins)
    ss <- which(rank(ins)%in%seq(from=nbins-2,to=nbins)) ## pick the top 3 intensities
    dh[["mids"]][ss]
  } 
  output$graph4 <- renderPlot({
    dataset <- datasetInput() %>%
      mutate(Weekend = ifelse(Weekend == 1, "Yes", "No")) %>%
      mutate(Holiday = ifelse(Holiday == 1, "Yes", "No")) %>%
      filter(Start.station == input$Station | End.station == input$Station) %>%
      filter(Month == input$Month) %>%
      filter(Weekend == input$Weekend) 
    peaks <- peakfinder(dataset$Hour)
    hist(dataset$Hour, main = "Popular Times", ylab = "Number of Flows", xlab = "Hours",col="lightblue")
    #sapply(peaks,function(x) abline(v=x,col="red"))
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)
