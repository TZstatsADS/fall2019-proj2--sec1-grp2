server <- function(input,output, session){

 dataSource <- reactive({switch(input$year, "1995" = df_1995, "2005" = df_2005, "2015" = df_2015)})

  
  greenLeafIcon <- makeIcon(
    iconUrl = "./www/tree.png",
    iconWidth = 18, iconHeight = 20,
    iconAnchorX = 22, iconAnchorY = 94
  )
  # 
  output$mymap <- renderLeaflet({
    df <- data()
    m <- leaflet(data = dataSource()) %>%
      addTiles()
    m
  })
  # 
  # output$plot=renderPlot({
  #   hist(faithful$eruptions, probability = TRUE, breaks = as.numeric(input$n_breaks),
  #        xlab = "Duration (minutes)", main = "Geyser eruption duration")
  # 
  #   dens <- density(faithful$eruptions, adjust = input$bw_adjust)
  #   lines(dens, col = "blue")
  # })

  
  output$plot=renderPlot({
    if(input$select_statistical == 'Largest tree size by region'){
      processed_df %>%
      group_by(borough)%>%
      summarise(avg_dbh = mean(tree_dbh, na.rm = TRUE))%>%
      arrange(avg_dbh) %>%
      ggplot(., aes(x= reorder(borough, avg_dbh), y=avg_dbh)) +
      geom_bar(stat='identity') +
      ylab("Avg of Tree Diameter Measured") + xlab ("") +
      ggtitle("largest tree size by region") +
      theme_minimal() +
      scale_x_discrete(labels = function(labels) {
        sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))
      })
    }
    else if(input$select_statistical == 'Which regions has most trees'){
      processed_df%>% 
      group_by(borough)%>% 
      summarise(rate = n()/nrow(processed_df))%>%
      arrange(rate)%>%
      ggplot(., aes(x= reorder(borough, rate), y=rate)) +
      geom_bar(stat='identity') + 
      ylab("trees count by regions") + xlab ("") + 
      ggtitle("Which regions has most trees") + 
      theme_minimal() + 
      scale_x_discrete(labels = function(labels) {
        sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))
      })
    }
    else if(input$select_statistical == "Health status across boroughs"){
      processed_df %>% 
        group_by(borough,status)%>% 
        summarise(rate = n()/nrow(processed_df))%>%
        arrange(borough,status)%>%
        ggplot(., aes(x= reorder(borough, rate), y=rate, fill = factor(status))) +
        stat_summary(fun.y=mean, geom="bar",position=position_dodge(1)) + theme_minimal()
    }
    else if(input$select_statistical == "Alive tree by tree type"){
      processed_df[processed_df$status == 'Alive',]%>% 
        group_by(spc_common)%>% 
        summarise(rate = n()/nrow(processed_df))%>%
        arrange(desc(rate))%>%
        slice(1:10) %>%
        ggplot(., aes(x= reorder(spc_common, rate), y=rate)) +
        geom_bar(stat='identity') + 
        ylab("Tree count percentage") + xlab ("") + 
        ggtitle("Alive tree by tree type") + 
        theme_minimal() + 
        scale_x_discrete(labels = function(labels) {
          sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))
        })+
        coord_flip()
    }
    else if(input$select_statistical == "Health condition for trees"){
      processed_df_health <- processed_df[processed_df$health != "",]
      processed_df_health%>%
        group_by(health)%>%
        summarise(rate = n()/nrow(processed_df_health))%>%
        ggplot(., aes(x= reorder(health, rate), y=rate)) +
        geom_bar(stat='identity') + 
        ylab("Tree count percentage") + xlab ("") + 
        ggtitle("Alive tree by tree type") + 
        theme_minimal() + 
        scale_x_discrete(labels = function(labels) {
          sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))
        })
    }
  })

  
  # Function
  # polution 
  # observeEvent(input$pollutant, {
  #   chosen_data <- pollution_data %>%
  #     filter(year == input$year) %>%
  #     filter(pollutant == input$pollutant)
  #   pal <- colorNumeric(palette = colorRampPalette(c("green", "red"))(10),
  #                       domain = chosen_data$measure)
  #   leafletProxy("mymap", data = chosen_data) %>%
  #   addProviderTiles("CartoDB.Positron") %>% # comment --- another style
  #   addCircles(lng = chosen_data$long, lat = chosen_data$lat, 
  #              color = pal(chosen_data$measure), radius = 2, opacity = 0.1)
  # })
  
  
  observeEvent(input$neighbour, {
    if(input$neighbour == "Enable"){
      tree_count <- dataSource() %>%
        group_by(nta, .drop = F) %>%
        tally()
      combined <- geo_join(nbhood, tree_count, by_sp = "ntacode", by_df = "nta")
      quantile(tree_count$n)
      pal <- colorBin(c("gray", colorRampPalette(c("lightgreen", "darkgreen"))(5)), 
                      bins = c(0, 1, seq(10, 50, by = 10)))
      
      leafletProxy("mymap", data = combined) %>%
        addProviderTiles("CartoDB.Positron") %>%
        clearMarkers() %>%
        clearControls() %>%
        setView(lat = NYC_coord["lat"], lng = NYC_coord["lon"], zoom = 10) %>%
        addPolygons(fillColor = ~pal(n), color = "black", weight = 1, fillOpacity = 0.8) %>%
        addLegend(pal = pal, values = c(0, 1, seq(10, 50, by = 10)))
      a <- labelFormat(prefix = "", suffix = "", between = " &ndash; ",
                       digits = 3, big.mark = ",", transform = identity)
      
    }
    else if(input$neighbour == "Disable"){
      filtered <- dataSource()%>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        clearControls() %>%
        clearShapes() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    })
  
  observeEvent(input$reset, {
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      clearControls() %>%
      clearShapes()
  })
  
  
  # Function 
  # Tree types 
  observeEvent(input$select_treetype, {

    if(input$select_treetype == "All")
    {
    
      popup <- paste("<strong>Type: </strong>", tree$spc_common,
                     "<br><strong>Status: </strong>", tree$status,
                     "<br><strong>Address: </strong>", tree$address)
      filtered <- dataSource()
      leafletProxy("mymap", data = filtered) %>%
        addProviderTiles("CartoDB.Positron") %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                 lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "None")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "American elm")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "American linden")
    {

      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Amur maple")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Ash")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Atlantic white cedar")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Black cherry")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Black oak")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Callery pear")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Chinese fringetree")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Crab apple")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Crepe myrtle")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Douglas-fir")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Eastern redcedar")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Ginkgo")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Honeylocust")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Japanese zelkova")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Kentucky yellowwood")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "London planetree")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Mulberry")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Northern red oak")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Norway maple")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Ohio buckeye")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Pignut hickory")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Pin oak")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Red maple")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Sawtooth oak")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Scarlet oak")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Silver linden")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Silver maple")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Sophora")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Southern magnolia")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Swamp white oak")
    {
      filtered <- dataSource %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Sweetgum")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Sycamore maple")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Tulip-poplar")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Turkish hazelnut")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "White oak")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
    else if(input$select_treetype == "Willow oak")
    {
      filtered <- dataSource() %>% filter(spc_common == input$select_treetype)
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon, popup = popup)
    }
  })
  
  
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
  
  
  observeEvent(input$borough,{
    b <- input$borough
    
    if(b == "All"){
      popup <- paste("<strong>Type: </strong>", tree$spc_common,
                     "<br><strong>Status: </strong>", tree$status,
                     "<br><strong>Address: </strong>", tree$address)
      
      leafletProxy("mymap") %>%
        clearMarkers() %>%
        clearShapes() %>%
        flyTo(lat = NYC_coord["lat"], lng = NYC_coord["lon"], zoom = 10) %>%
        addPolygons(data = combined, color = "black", weight = 1, 
                    fillOpacity = 0) %>%
        addMarkers(lng = dataSource()$longitude, lat = dataSource()$latitude, popup = popup, icon = greenLeafIcon)
      
    }
    
    else{
      filtered <- dataSource() %>%
        filter(borough == b)
      
      popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                     "<br><strong>Status: </strong>", filtered$status,
                     "<br><strong>Address: </strong>", filtered$address)
      
      centroid <- gCentroid(combined[combined@data$boro_name == b, ])
      
      leafletProxy("mymap") %>%
        clearMarkers() %>%
        clearShapes() %>%
        flyTo(lng = centroid@coords[1], lat = centroid@coords[2], zoom = 11) %>%
        addPolygons(data = combined, color = "black", weight = 1, 
                    fillOpacity = 0) %>%
        addMarkers(lng = filtered$longitude, lat = filtered$latitude,
                   popup = popup, icon = greenLeafIcon) %>%
        addPolygons(data = combined[combined@data$boro_name == b, ],
                    color = "lightblue", weight = 3,
                    fillColor = "lightgreen", fillOpacity = 0.2)
    }
    
  })
  
  
  observeEvent(input$count, {
    if(input$count == "n"){
      bins <- set_bins(combined@data$n, 4)
      pal <- set_pal(bins)
      legend_labels <- set_labels(bins)
      p1 <- leafletProxy("mymap")  %>%
        clearMarkers() %>%
        clearControls() %>%
        clearShapes() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lat = NYC_coord["lat"], lng = NYC_coord["lon"], zoom = 10) %>%
        
        ###################### Polygons
        
        addPolygons(fillColor = ~pal(n), color = "black", weight = 1, 
                    fillOpacity = 0.8, popup = popup[[1]][[1]], 
                    data = data_by_borough[[1]],
                    group = "Manhattan") %>%
        addPolygons(fillColor = ~pal(n), color = "black", weight = 1,
                    fillOpacity = 0.8, popup = popup[[2]][[1]],
                    data = data_by_borough[[2]],
                    group = "Brooklyn") %>%
        addPolygons(fillColor = ~pal(n), color = "black", weight = 1,
                    fillOpacity = 0.8, popup = popup[[3]][[1]],
                    data = data_by_borough[[3]],
                    group = "Queens") %>%
        addPolygons(fillColor = ~pal(n), color = "black", weight = 1,
                    fillOpacity = 0.8, popup = popup[[4]][[1]],
                    data = data_by_borough[[4]],
                    group = "Staten Island") %>%
        addPolygons(fillColor = ~pal(n), color = "black", weight = 1,
                    fillOpacity = 0.8, popup = popup[[5]][[1]],
                    data = data_by_borough[[5]],
                    group = "Bronx") %>%
        
        #########################
      
      addLegend("topleft",
                pal = pal, values = bins, opacity = 0.8, 
                labFormat = function(type, cuts, p) paste(legend_labels),
                title = legend_title[1], group = "legend") %>%
        
        addLayersControl(overlayGroups = c("Manhattan", "Brooklyn", "Queens",
                                           "Staten Island", "Bronx"),
                         options = layersControlOptions(collapsed = F))
      
    }

    else if(input$count == "density"){
      bins <- seq(min(combined@data$tree_per_km2, na.rm = T), 
                  max(combined@data$tree_per_km2, na.rm = T), length.out = 5)
      pal <- set_pal(bins)
      #legend_labels <- set_labels(bins)
      p1 <- leafletProxy("mymap") %>%
        addProviderTiles("CartoDB.Positron") %>%
        clearMarkers() %>%
        clearControls() %>%
        clearShapes() %>%
        setView(lat = NYC_coord["lat"], lng = NYC_coord["lon"], zoom = 10) %>%
        
        ###################### Polygons
        
        addPolygons(fillColor = ~pal(tree_per_km2), color = "black", weight = 1, 
                    fillOpacity = 0.8, popup = popup[[1]][[2]], 
                    data = data_by_borough[[1]],
                    group = "Manhattan") %>%
        addPolygons(fillColor = ~pal(tree_per_km2), color = "black", weight = 1,
                    fillOpacity = 0.8, popup = popup[[2]][[2]],
                    data = data_by_borough[[2]],
                    group = "Brooklyn") %>%
        addPolygons(fillColor = ~pal(tree_per_km2), color = "black", weight = 1,
                    fillOpacity = 0.8, popup = popup[[3]][[2]],
                    data = data_by_borough[[3]],
                    group = "Queens") %>%
        addPolygons(fillColor = ~pal(tree_per_km2), color = "black", weight = 1,
                    fillOpacity = 0.8, popup = popup[[4]][[2]],
                    data = data_by_borough[[4]],
                    group = "Staten Island") %>%
        addPolygons(fillColor = ~pal(tree_per_km2), color = "black", weight = 1,
                    fillOpacity = 0.8, popup = popup[[5]][[2]],
                    data = data_by_borough[[5]],
                    group = "Bronx") %>%
        #########################
      
      addLegend("topleft",
                pal = pal, values = bins, opacity = 0.8, 
                labFormat = labelFormat(),
                title = legend_title[2], group = "legend") %>%
        
        addLayersControl(overlayGroups = c("Manhattan", "Brooklyn", "Queens",
                                           "Staten Island", "Bronx"),
                         options = layersControlOptions(collapsed = F))
    }
  })
}
  



