server <- function(input,output, session){
  
  data <- reactive({
    x <- df
  })
  makeLeafIcon <- function(data){
    if(data$status =='Alive'){
      greenLeafIcon <- makeIcon(
    
          iconUrl = "../lib/leaf-green.png",
          iconWidth = 18,
          iconHeight = 20,
          iconAnchorX = 22,
          iconAnchorY = 94
    )}else if(data$status == 'Stump'){  
        greenLeafIcon <- makeIcon(
        iconUrl = "../lib/leaf-orange.png",
        iconWidth = 18,
        iconHeight = 20,
        iconAnchorX = 22,
        iconAnchorY = 94
      )}else if(data$status == 'Dead'){
          greenLeafIcon <- makeIcon(
            iconUrl = "../lib/leaf-red.png",
            iconWidth = 18,
            iconHeight = 20,
            iconAnchorX = 22,
            iconAnchorY = 94)}
    }
  output$mymap <- renderLeaflet({
    df <- data()
    
    m <- leaflet(data = df_2015) %>%
      addTiles() 
      # addMarkers(lng = ~longitude,
      #            lat = ~latitude, icon = greenLeafIcon)
    m
  })
  # output$plot=renderPlot({
  #   hist(faithful$eruptions, probability = TRUE, breaks = as.numeric(input$year),
  #        xlab = "Duration (minutes)", main = "Geyser eruption duration")
  #   
  #   dens <- density(faithful$eruptions, adjust = input$bw_adjust)
  #   lines(dens, col = "blue")
  # })
  
  queryEvent <-function(select_treetype, filtered){
    choices = c("All", "American elm", "American linden", "Amur maple", "Ash", "Atlantic white cedar", "Black cherry", "Black oak", 
                "Callery pear", "Chinese fringetree", "Crab apple", "Crepe myrtle", "Douglas-fir", "Eastern redcedar", "Ginkgo", "Hedge maple", 
                "Honeylocust", "Japanese zelkova", "Kentucky yellowwood", "London planetree", "Mulberry", "Northern red oak", "Norway maple", "Ohio buckeye", 
                "Pignut hickory", "Pin oak", "Red maple", "Sawtooth oak", "Scarlet oak", "Silver linden", "Silver maple", "Sophora", 
                "Southern magnolia", "Swamp white oak", "Sweetgum", "Sycamore maple", "Tulip-poplar", "Turkish hazelnut", "White oak", "Willow oak") 
    if(select_treetype %in% choices){
      leafletProxy("mymap", data = filtered) %>%
              clearMarkers() %>%
              addMarkers(lng = filtered$longitude,
                       lat = filtered$latitude, icon = makeLeafIcon(filtered))
    }
  }
  
  
  observeEvent(input$select_treetype,queryEvent(input$select_treetype, filtered = df_2015) )
  
  
  
  # Function 
  # Tree types 
  # observeEvent(input$select_treetype, {
  #   if(input$select_treetype == "All")
  #   {
  #     filtered <- df_2015
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                lat = filtered$latitude, icon = greenLeafIcon(filtered))
  #   }
  #   else if(input$select_treetype == "American elm")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon(filtered))
  #   }
  #   else if(input$select_treetype == "American linden")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon(filtered))
  #   }
  #   else if(input$select_treetype == "Amur maple")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Ash")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Atlantic white cedar")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Black cherry")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Black oak")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Callery pear")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Chinese fringetree")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Crab apple")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Crepe myrtle")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Douglas-fir")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Eastern redcedar")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Ginkgo")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Honeylocust")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Japanese zelkova")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Kentucky yellowwood")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "London planetree")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Mulberry")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Northern red oak")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Norway maple")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Ohio buckeye")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Pignut hickory")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Pin oak")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Red maple")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Sawtooth oak")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Scarlet oak")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Silver linden")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Silver maple")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Sophora")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Southern magnolia")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Swamp white oak")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Sweetgum")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Sycamore maple")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Tulip-poplar")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Turkish hazelnut")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "White oak")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  #   else if(input$select_treetype == "Willow oak")
  #   {
  #     filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
  #     leafletProxy("mymap", data = filtered) %>%
  #       clearMarkers() %>%
  #       addMarkers(lng = filtered$longitude,
  #                  lat = filtered$latitude, icon = greenLeafIcon)
  #   }
  # })
  # 
    

  
  # Function 
  # Change the boroughs 
  observeEvent(input$select_borough, {
    if(input$select_borough == 'All'){
      setView(map = leafletProxy("mymap"), lng=-73.9712, lat=40.7831, zoom=10)
    }
    else if(input$select_borough == 'Manhattan'){
      setView(map = leafletProxy("mymap"), lng=-73.9712, lat=40.7831, zoom=12)
    }
    else if(input$select_borough == 'Queens'){
      setView(map = leafletProxy("mymap"), lng=-73.7949, lat=40.7282, zoom=12)
    }
    else if(input$select_borough == 'Brooklyn'){
      setView(map = leafletProxy("mymap"), lng=-73.9442, lat=40.6782, zoom=12)
    }
    else if(input$select_borough == "Staten Island"){
      setView(map = leafletProxy("mymap"), lng=-74.1502, lat=40.5795, zoom=12)
    }
    else{
      setView(map = leafletProxy("mymap"), lng=-73.8648, lat=40.8448, zoom=12)
    }
  })
  

}