ui <- fluidPage(
  
  sidebarPanel(
    selectInput("year", label = "Year:",
                choices = c(2015, 2005, 1995)),
    
    # sliderInput("bw_adjust", label = "Bandwidth adjustment:",
    #             min = 0.2, max = 2, value = 1, step = 0.2),
    
    selectInput("select_borough", "Boroughs", 
                choices = c("All", "Manhattan", "Brooklyn", "Queens",
                            "Staten Island", "The Bronx"), selected = "All"), 
      
    selectInput("select_treetype", "Tree types", 
                choices = c("All", "American elm", "American linden", "Amur maple", "Ash", "Atlantic white cedar", "Black cherry", "Black oak", 
                            "Callery pear", "Chinese fringetree", "Crab apple", "Crepe myrtle", "Douglas-fir", "Eastern redcedar", "Ginkgo", "Hedge maple", 
                            "Honeylocust", "Japanese zelkova", "Kentucky yellowwood", "London planetree", "Mulberry", "Northern red oak", "Norway maple", "Ohio buckeye", 
                            "Pignut hickory", "Pin oak", "Red maple", "Sawtooth oak", "Scarlet oak", "Silver linden", "Silver maple", "Sophora", 
                            "Southern magnolia", "Swamp white oak", "Sweetgum", "Sycamore maple", "Tulip-poplar", "Turkish hazelnut", "White oak", "Willow oak"), selected = "All")), 
    checkboxInput("health_status", label = "Choice A", value = TRUE)
  mainPanel(
    leafletOutput("mymap",height = 800) 
    # plotOutput("plot")
  )
)

