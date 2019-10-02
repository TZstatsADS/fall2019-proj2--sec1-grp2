#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(leaflet)
library(dplyr)
# 
# df_2015 <- readRDS("../data/sample_data_2015.rds")
# df_2005 <- readRDS("../data/sample_data_2005.rds")
# df_1995 <- readRDS("../data/sample_data_1995.rds")
# 

treeIcons <- iconList(
  Dead = makeIcon(iconUrl = "../data/dead-tree.png",  
                 iconWidth = 18,
                 iconHeight = 20,
                 iconAnchorX = 22,
                 iconAnchorY = 94),
  Alive = makeIcon(iconUrl = "../data/leaf-green.png",  
                   iconWidth = 18,
                   iconHeight = 20,
                   iconAnchorX = 22,
                   iconAnchorY = 94),
  Stump = makeIcon(iconUrl = "../data/stump.png",  
                    iconWidth = 18,
                    iconHeight = 20,
                    iconAnchorX = 22,
                    iconAnchorY = 94)
)

healthIcons <- iconList(
  Good = makeIcon(iconUrl = "../data/leaf-green.png",  
                  iconWidth = 18,
                  iconHeight = 20,
                  iconAnchorX = 22,
                  iconAnchorY = 94),
  Fair = makeIcon(iconUrl = "../data/leaf-orange.png",  
                   iconWidth = 18,
                   iconHeight = 20,
                   iconAnchorX = 22,
                   iconAnchorY = 94),
  Poor = makeIcon(iconUrl = "../data/leaf-red.png",  
                   iconWidth = 18,
                   iconHeight = 20,
                   iconAnchorX = 22,
                   iconAnchorY = 94)
)


# Define server logic required to draw a histogram
shinyServer <- function(input, output, session) {
  # Activating a reactive env
  data <- eventReactive(input$go,{
    readRDS(paste("../data/",input$year, sep = ""))},
    ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    df <- data()
    m <- leaflet(data = df) %>% addTiles() %>% setView(-74.00, 40.71, zoom = 12)
  })
  
  observeEvent(input$tree_type, {
    df = data()
    filtered <- df %>% filter(spc_common %in% input$tree_type)
    leafletProxy("mymap", data = filtered) %>%
      clearMarkers() %>%
      addMarkers(lng = ~longitude,
                 lat = ~latitude, icon = ~treeIcons[status])
  })

  observeEvent(input$status_status, {
    df = data()
    filtered <- df %>% filter(status %in% input$status_status)
    leafletProxy("mymap", data = filtered) %>%
      clearMarkers() %>%
      addMarkers(lng = ~longitude,
                 lat = ~latitude, icon = ~treeIcons[status])
  })
  
  observeEvent(input$health_status,{
    df = data()
    
    filtered <- df %>% filter(status == "Alive") %>%
      filter(health %in% input$health_status)
    leafletProxy("mymap", data = filtered) %>%
      clearMarkers() %>%
      addMarkers(lng = ~longitude,
                   lat = ~latitude, icon = ~healthIcons[health])
  })

  
  
  observeEvent(input$boro, {
    if(input$boro[1] == 'Manhattan'){
      setView(map = leafletProxy("mymap"), lng=-73.9712, lat=40.7831, zoom=12)
    }
    else if(input$boro[1] == 'Queens'){
      setView(map = leafletProxy("mymap"), lng=-73.7949, lat=40.7282, zoom=12)
    }
    else if(input$boro[1] == 'Brooklyn'){
      setView(map = leafletProxy("mymap"), lng=-73.9442, lat=40.6782, zoom=12)
    }
    else if(input$boro[1] == "Staten Island"){
      setView(map = leafletProxy("mymap"), lng=-74.1502, lat=40.5795, zoom=12)
    }
    else{
      setView(map = leafletProxy("mymap"), lng=-73.8648, lat=40.8448, zoom=12)
    }
  })

}
 


