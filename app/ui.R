ui <- fluidPage(
  setBackgroundImage(
    src = "background_1.jpg"
  ),
  sidebarPanel(
      tabsetPanel(id = "tabset",
        tabPanel( "Trees", 
          helpText("Choose tree information in different years"),
          selectInput("year", label = "Year:",
                      choices = c(2015, 2005, 1995)),
        
          sliderInput("slider1", h5("Year"),
                      min = 1995, max = 2015, value = 1995,
                      step = 10, animate = TRUE,
                      pre = 'Year'),
          
          
          # sliderInput("bw_adjust", label = "Bandwidth adjustment:",
          #             min = 0.2, max = 2, value = 1, step = 0.2),
          helpText("Choose different boroughs"),
          selectInput("select_borough", "Boroughs", 
                      choices = c("All", "Manhattan", "Brooklyn", "Queens",
                                  "Staten Island", "The Bronx"), selected = "All"), 
          
          helpText("A-Z different tree types"),
          selectInput("select_treetype", "Tree types", 
                      choices = c("All", "American elm", "American linden", "Amur maple", "Ash", "Atlantic white cedar", "Black cherry", "Black oak", 
                                  "Callery pear", "Chinese fringetree", "Crab apple", "Crepe myrtle", "Douglas-fir", "Eastern redcedar", "Ginkgo", "Hedge maple", 
                                  "Honeylocust", "Japanese zelkova", "Kentucky yellowwood", "London planetree", "Mulberry", "Northern red oak", "Norway maple", "Ohio buckeye", 
                                  "Pignut hickory", "Pin oak", "Red maple", "Sawtooth oak", "Scarlet oak", "Silver linden", "Silver maple", "Sophora", 
                                  "Southern magnolia", "Swamp white oak", "Sweetgum", "Sycamore maple", "Tulip-poplar", "Turkish hazelnut", "White oak", "Willow oak"), selected = "All")), 
          # h2("Trees"),
          # p("Trees Trees Trees"),
          # 
          # img(src="trees_selector.png", height = 271, width = 500),
        
      tabPanel( "Pollutants", 
        selectInput("pollutant", "Pollutants", 
                    choices = c("PM2.5" = "pm", "Nitrogen Dioxide (NO2)" = "no2", 
                                "Nitric Oxide (NO)" = "no", "Black Carbon" = "bc"),
                    selected = "PM 2.5")),  
        # selectInput("n_breaks", label = "Number of bins:",
        #             choices = c(10, 20, 35, 50), selected = 20),
        # 
        # sliderInput("bw_adjust", label = "Bandwidth adjustment:",
        #             min = 0.2, max = 2, value = 1, step = 0.2)),
      tabPanel("Statistical analysis", 
               selectInput("select_statistical", "Statistical analysis",
                           choices = c("Largest tree size by region", "Which regions has most trees", "Health status across boroughs", "Alive tree by tree type",
                                       "Health condition for trees"), selected = "Largest tree size by region"))
      ), 
      actionButton("reset", "Reset")), 

  mainPanel(
    leafletOutput("mymap",height = 800), 
    plotOutput("plot")
    # plotOutput("plot")
  )
)

