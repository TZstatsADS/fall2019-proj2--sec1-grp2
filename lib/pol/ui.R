ui <- fluidPage(
    
    sidebarPanel(
        selectInput("year", label = "Year",
                    choices = 2009:2017, selected = 2017),
        
        # sliderInput("bw_adjust", label = "Bandwidth adjustment:",
        #             min = 0.2, max = 2, value = 1, step = 0.2),
        
        selectInput("borough", "Boroughs", 
                    choices = c("All", "Manhattan", "Brooklyn", "Queens",
                                "Staten Island", "The Bronx"), selected = "All"), 
        
        selectInput("pollutant", "Pollutants", 
                    choices = c("PM2.5" = "pm", "Nitrogen Dioxide (NO2)" = "no2", 
                                "Nitric Oxide (NO)" = "no", "Black Carbon" = "bc"),
                    selected = "PM 2.5")),
        
    mainPanel(
        leafletOutput("mymap",height = 800) 
        # plotOutput("plot")
    )
    
)
    
    