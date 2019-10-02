server <- function(input,output, session){
  
  data <- reactive({
    x <- df
  })
  
  greenLeafIcon <- makeIcon(
    iconUrl = "./www/tree.png",
    iconWidth = 18, iconHeight = 20,
    iconAnchorX = 22, iconAnchorY = 94
  )
  # 
  output$mymap <- renderLeaflet({
    df <- data()
    m <- leaflet(data = df_2015) %>%
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
  observeEvent(input$pollutant, {
    chosen_data <- pollution_data %>%
      filter(year == input$year) %>%
      filter(pollutant == input$pollutant)
    pal <- colorNumeric(palette = colorRampPalette(c("green", "red"))(10),
                        domain = chosen_data$measure)
    leafletProxy("mymap", data = chosen_data) %>%
    addProviderTiles("CartoDB.Positron") %>% # comment --- another style
    addCircles(lng = chosen_data$long, lat = chosen_data$lat, 
               color = pal(chosen_data$measure), radius = 2, opacity = 0.1)
  })
  
  # Function 
  # Tree types 
  observeEvent(input$select_treetype, {
    if(input$select_treetype == "All")
    {
      filtered <- df_2015
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                 lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "American elm")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "American linden")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Amur maple")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Ash")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Atlantic white cedar")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Black cherry")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Black oak")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Callery pear")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Chinese fringetree")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Crab apple")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Crepe myrtle")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Douglas-fir")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Eastern redcedar")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Ginkgo")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Honeylocust")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Japanese zelkova")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Kentucky yellowwood")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "London planetree")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Mulberry")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Northern red oak")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Norway maple")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Ohio buckeye")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Pignut hickory")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Pin oak")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Red maple")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Sawtooth oak")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Scarlet oak")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Silver linden")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Silver maple")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Sophora")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Southern magnolia")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Swamp white oak")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Sweetgum")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Sycamore maple")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Tulip-poplar")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Turkish hazelnut")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "White oak")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$select_treetype == "Willow oak")
    {
      filtered <- df_2015 %>% filter(spc_common == input$select_treetype)
      leafletProxy("mymap", data = filtered) %>%
        clearMarkers() %>%
        addMarkers(lng = filtered$longitude,
                   lat = filtered$latitude, icon = greenLeafIcon)
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
  
}
  
  


