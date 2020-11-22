

full_20 = read.csv("~/Desktop/bikes_2020_full.csv")


ui <- fluidPage(
  theme = shinytheme("cosmo"),
  navbarPage(
    # Application title
    title="Bike Sharing in Washington DC",
    
    tabPanel("About", 
             
             sidebarPanel(
               
               h3("Introduction:"), textOutput("intro"),
               
               selectInput("dataset", "Choose a Model:",
                           choices = c("Covid Days", "Usual Days")),
               
               actionButton("update", "Update Model"),
               
               helpText("Note: click to view the updates whenever you choose a new model.")
               ),
             
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
    
    #third tab
    
    tabPanel("Predict Bikes", 
             sidebarPanel(
               # Add user inputs here
               
               actionButton(inputId = "action", label = "Predict!")),
             
             
             mainPanel(h3("Predicted Number of Bikes"),
                       verbatimTextOutput("bikes"),
                       h3("Map showing nearest 3 stations"),
                       verbatimTextOutput("Map1"))),
    
    #fourth tab
    
    tabPanel("Potential Rush Hours",
             sidebarPanel("Popular Times",
                          selectInput("Station", "Choose a Station:",
                                      choices = levels(full_20$Start.station)),
                          
                          selectInput("Month", "Choose a Month:",
                                      choices = levels(as.factor(full_20$Month))),
                          
                          selectInput("Weekend", "Is it a Weekend?",
                                      choices = c("Yes", "No"))
                          ),
             mainPanel(plotOutput("graph4"))),
    
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
  
  #data input where user can select which data to look at
  datasetInput <- eventReactive(input$update, {
    switch(input$dataset,
           "Covid Days" = full_20,
           "Usual Days" = full_19
    )
  }, ignoreNULL = FALSE)
  
  # first tab
  output$intro= renderText(
    "Our topic is about: ")
  
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
  
  output$graph4 <- renderPlot({
    dataset <- datasetInput() %>%
      mutate(Weekend = ifelse(Weekend == 1, "Yes", "No")) %>%
      mutate(Holiday = ifelse(Holiday == 1, "Yes", "No")) %>%
      filter(Start.station == input$Station | End.station == input$Station) %>%
      filter(Month == input$Month) %>%
      filter(Weekend == input$Weekend) 
    hist(dataset$Hour, main = "Popular Times", ylab = "Number of Flows", xlab = "Hours")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)