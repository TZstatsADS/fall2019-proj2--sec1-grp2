library(leaflet)
library(ggmap)
library(shiny)
library(sp)
library(raster)
library(rgdal)

load("../../data/pollution.RData")


NYC_coord <- c(lon = -74.00597, lat = 40.71278)

server <- function(input, output){
    output$mymap <- renderLeaflet({
        
        chosen_data <- pollution_data %>%
            filter(year == input$year) %>%
            filter(pollutant == input$pollutant)
        
        pal <- colorNumeric(palette = colorRampPalette(c("green", "red"))(10),
                            domain = chosen_data$measure)
        
        p1 <- leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            setView(lat = NYC_coord["lat"], lng = NYC_coord["lon"], zoom = 10) %>%
            addCircles(lng = chosen_data$long, lat = chosen_data$lat, 
                       color = pal(chosen_data$measure), radius = 2, opacity = 0.1)
        
        p1
    })
}
