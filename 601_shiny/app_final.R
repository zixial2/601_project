
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


# read data for model prediction and plot

full_20 = read.csv("./data/bikes_2020_full.csv") %>% dplyr::filter(Month<=10) %>% na.omit()
b = full_20%>% select(Start.station,Hour) %>% group_by(Start.station)%>%arrange(Start.station) %>%filter(Start.station!="")
c = unique(b) %>%  arrange(Start.station,Hour) # filter stations
c$h = TRUE
for (i in unique(c$Start.station)){
  if (length(c[c$Start.station==i,]$Hour)!=24)c[c$Start.station==i,]$h = FALSE} # FILTER out stations that do not have 24 hours available in the data 

full_19 = read.csv("./data/bike_2019_full.csv") %>% na.omit()
nearest_stations  = read.csv("./data/nearest_stations.csv")

# shiny 
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
               pickerInput("sta", "Choose a Station:",  options = list(`live-search` = TRUE), choices = levels(c$Start.station),selected = "Columbus Circle / Union Station"),
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
                       h3("Predicted Number of Bikes for the nearest 3 stations"),
                       tableOutput("view"))),
    
    tabPanel("Potential Rush Hours",
             sidebarPanel("Popular Times",
                          
                          selectInput("dataset", "Choose a Period:",
                                      choices = c("Covid Days", "Usual Days")),
                          
                          actionButton("update", "Update Period"),
                          
                          helpText("Note: click to view the updates."),
                          selectInput("Station", "Choose a Station:",
                                      choices = levels(full_20$Start.station), selected = "Columbus Circle / Union Station"),
                          
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
    pred = predict(model(), new_data())
    num = max(0,round(pred))
    print(paste0("There will be ", num, " bikes on ", input$mon,"/",input$day," from ", input$hour,":00 to ", as.numeric(input$hour)+1,":00","\nat station ",input$sta))
  })
  
  output$bikes = renderText({
      #print(predictVal())
      #num = max(0,round(predictVal()))
      #print(paste0("There will be ", num, " bikes on ", input$mon,"/",input$day," from ", input$hour,":00 to ", as.numeric(input$hour)+1,":00","\n at station ",input$sta))
      predictVal()
    })
  
  # Nearest Stations 
  #fit model to three nearest stations
  
  stations_3_list <- reactive({nearest_stations %>%
      filter(start_station_name == input$sta) })
  
  stations_list <- reactive({as.character(stations_3_list()$Near_Station)})
  
  sta_1 <- reactive({stations_list()[1]})
  sta_2 <- reactive({stations_list()[2]})
  sta_3 <- reactive({stations_list()[2]})
  
  #first station
  try_data_sta_1 <- reactive({
    filter(full_20, Start.station == sta_1() |End.station == sta_1())
  })
  
  initial_sta_1 <- reactive({real_data %>%
      filter(name == sta_1()) %>%
      select(capacity) %>%
      as.numeric()  })
  try_data_1_sta_1 =  reactive({    
    try_data_sta_1 ()%>%
      mutate(flow = ifelse(Start.station == sta_1(),-1, 1))%>%
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
  
  model_sta_1 =reactive({lm(cumsum ~ as.factor(Hour) + Weekend + Holiday + as.factor(Month) + Day, data =  try_data_1_sta_1())})
  
  new_data_sta_1 = reactive({data.frame(Hour=input$hour, Weekend=input$week, Holiday = input$holi,Month=as.character(ifelse(as.numeric(input$mon)>=10,"10",input$mon)), Day=as.numeric(input$day))})
  
  predictVal_sta_1 = eventReactive(input$action,{
    predict(model_sta_1(), new_data_sta_1())
  })
  
  #second station
  try_data_sta_2 <- reactive({
    filter(full_20, Start.station == sta_2() |End.station == sta_2())
  })
  
  initial_sta_2 <- reactive({real_data %>%
      filter(name == sta_2()) %>%
      select(capacity) %>%
      as.numeric()  })
  try_data_1_sta_2 =  reactive({    
    try_data_sta_2 ()%>%
      mutate(flow = ifelse(Start.station == sta_2(),-1, 1))%>%
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
  
  model_sta_2 =reactive({lm(cumsum ~ as.factor(Hour) + Weekend + Holiday + as.factor(Month) + Day, data =  try_data_1_sta_2())})
  
  new_data_sta_2 = reactive({data.frame(Hour=input$hour, Weekend=input$week, Holiday = input$holi,Month=as.character(ifelse(as.numeric(input$mon)>=10,"10",input$mon)), Day=as.numeric(input$day))})
  
  predictVal_sta_2 = eventReactive(input$action,{
    predict(model_sta_2(), new_data_sta_2())
  })
  
  #third station
  try_data_sta_3 <- reactive({
    filter(full_20, Start.station == sta_3() |End.station == sta_3())
  })
  
  initial_sta_3 <- reactive({real_data %>%
      filter(name == sta_3()) %>%
      select(capacity) %>%
      as.numeric()  })
  try_data_1_sta_3 =  reactive({    
    try_data_sta_3 ()%>%
      mutate(flow = ifelse(Start.station == sta_3(),-1, 1))%>%
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
  
  model_sta_3 =reactive({lm(cumsum ~ as.factor(Hour) + Weekend + Holiday + as.factor(Month) + Day, data =  try_data_1_sta_3())})
  
  new_data_sta_3 = reactive({data.frame(Hour=input$hour, Weekend=input$week, Holiday = input$holi,Month=as.character(ifelse(as.numeric(input$mon)>=10,"10",input$mon)), Day=as.numeric(input$day))})
  
  predictVal_sta_3 = eventReactive(input$action,{
    predict(model_sta_3(), new_data_sta_3())
  })
  
  #get predicted numbers together
  predicted_numbers <- reactive({c(round(predictVal_sta_1()), round(predictVal_sta_2()), round(predictVal_sta_3()))})
  
  #output table
  
  output$view <- renderTable({
    dataset <- data.frame("Station" = stations_list(), "Predicted_Number"=predicted_numbers())
    head(dataset)
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
