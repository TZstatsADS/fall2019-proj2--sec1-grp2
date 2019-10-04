library(leaflet)
library(ggmap)
library(shiny)
library(sp)
library(raster)
library(rgdal)

ui <- fluidPage(
    
    sidebarPanel(
        #selectInput("year", label = "Year",
         #           choices = 2009:2017, selected = 2017),
        
        # sliderInput("bw_adjust", label = "Bandwidth adjustment:",
        #             min = 0.2, max = 2, value = 1, step = 0.2),
        selectInput("count", "Measure", 
                    choices = c("Total number of trees" = "n",
                                "Number of trees per KM2" = "density"),
                    selected = "Total number of trees")
        
        # selectInput("borough", "Boroughs", 
        #             choices = c("All", "Manhattan", "Brooklyn", "Queens",
        #                         "Staten Island", "Bronx"), selected = "All"), 
        # 
        # checkboxInput("fly", "Fly", value = FALSE)
    ),
        
    mainPanel(
        leafletOutput("map1",height = 800) 
    )
    
)
    
    
