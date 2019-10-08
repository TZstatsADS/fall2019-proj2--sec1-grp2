# TreeApp version2

# global.r

library(shinydashboard)
library(leaflet)
library(dplyr)
library(curl) 
library(ggplot2)
df = readRDS("../data/sample_data.rds")
load("../data/pollution.RData")
nbhood <- rgdal::readOGR("../data/nbhood/geo_export_63264cca-db33-43e7-ac15-9019c83788c0.shp")

NYC_coord <- c(lon = -74.00597, lat = 40.71278)
df$full_address <- mutate(df, full_address = paste(address, zip_city, state, postcode, sep = ', '))
df$nta <- factor(df$nta, levels = nbhood@data$ntacode)


# clickable trees ------------------------------------------------------------------------------------------------------
Round <- function(x, type = c("round", "ceiling", "floor")){
  i <- floor(log10(x))
  if(type == "round")
    output <- round(x/10^i * 2) / 2 * 10^i
  else if(type == "ceiling")
    output <- ceiling(x/10^i * 2) / 2 * 10^i
  else
    output <- floor(x/10^i * 2) / 2 * 10^i
  return(output)
}

set_bins <- function(count, n_bins){
  n <- n_bins + 1
  count <- count[!is.na(count)]
  
  if(length(count) < 1000){
    bins <- c(1, Round(seq(0, Round(max(count), "ceiling"), length.out = n)[-1], "round"))
    return(bins)
  }
  
  q <- quantile(count, probs = seq(0, 1, length.out = n))
  bins <- c(1, Round(q[2:(n-1)], "round"), Round(q[n], "ceiling"))
  return(bins)
}

set_pal <- function(bins){
  # pal <- colorBin(c("#808080", colorRampPalette(c("lightgreen", "darkgreen"))(length(bins) - 2)), 
  #                bins = bins)
  pal <- colorBin(colorRampPalette(c("lightgreen", "darkgreen"))(length(bins) - 1), 
                  bins = bins)
  return(pal)
}

set_labels <- function(bins){
  legend_labels <- c(paste(bins[1], "-", bins[2], sep = ""))
  for(i in 2:(length(bins)-1)){
    next_level <- paste(bins[i]+1, "-", bins[i+1], sep = "")
    legend_labels <- c(legend_labels, next_level)
  }
  return(legend_labels)
}





# Define server logic -----------------------------------------



server <- function(input, output,session) {
  
  
  data <- reactive({
    x <- df %>% filter(Year == input$year)
  })
  
  greenLeafIcon <- makeIcon(
    iconUrl = "../www/tree.png",
    iconWidth = 18, iconHeight = 20,
    iconAnchorX = 0, iconAnchorY = 0
  )
  
  output$treemap <- renderLeaflet({
    df <- data()
    m <- leaflet(data = data()) %>% addProviderTiles("CartoDB.Positron") %>% setView(-73.9712, 40.7831, zoom = 13)
    m
  })
  

  
  output$treeSelect <- renderUI({
    # Add names, so that we can add all=0
    df = data()
    tree_choices <- sort(unique(df$spc_common))
    selectInput("tree_type","Tree Type(s): ", tree_choices, selected = tree_choices, multiple = TRUE)
  })
  
  
  
  output$boroCountPlot <- renderPlot({
    df = data()
    tmp = df %>% group_by(borough)
  })
  
  output$boroSelect <- renderUI({
    df = data()
    boro_choices <- sort(unique(df$borough))
    selectInput("boro_type","NYC Borough(s): ", boro_choices, selected = "Manhattan", multiple = TRUE)
  })
  
  # For Borough Zoom Features
  output$boroSelect2 <- renderUI({
    df = data()
    boro_choices <- sort(unique(df$borough))
    selectInput("boro_zoom","Zoom to See NYC Borough(s): ", boro_choices, selected = "Manhattan", multiple = FALSE)
  })

  observeEvent(input$tree_type, {
    df = data()
    filtered <- df %>% filter(borough %in% input$boro_type, spc_common %in% input$tree_type)
    popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                   "<br><strong>Status: </strong>", filtered$status,
                   "<br><strong>Address: </strong>", filtered$address)
    leafletProxy("treemap", data = filtered) %>%
      clearMarkers() %>%
      addMarkers(lng = ~longitude,
                 lat = ~latitude, icon = greenLeafIcon, popup = popup)
  })
  
  
  
  observeEvent(input$boro_type, {
    df = data()
    filtered <- df %>% filter(borough %in% input$boro_type)
    leafletProxy("treemap", data = filtered) %>%
      clearMarkers() %>%
      addMarkers(lng = ~longitude,
                 lat = ~latitude, icon = greenLeafIcon)
  })
  
  observeEvent(input$boro_zoom, {
    if(input$boro_zoom == 'Manhattan'){
      setView(map = leafletProxy("treemap"), lng=-73.9712, lat=40.7831, zoom=13)
    }
    else if(input$boro_zoom== 'Queens'){
      setView(map = leafletProxy("treemap"), lng=-73.7949, lat=40.7282, zoom=13)
    }
    else if(input$boro_type== 'Brooklyn'){
      setView(map = leafletProxy("treemap"), lng=-73.9442, lat=40.6782, zoom=13)
    }
    else if(input$boro_type== "Staten Island"){
      setView(map = leafletProxy("treemap"), lng=-74.1502, lat=40.5795, zoom=13)
    }
    else{
      setView(map = leafletProxy("treemap"), lng=-73.8648, lat=40.8448, zoom=13)
    }
  })
  
  # filter pollution data
  
  pollution <- reactive({x<- pollution_data %>%
                            filter(year == input$year)})
  
  
  observeEvent(input$pollutant, {
    chosen_data <- pollution() %>% filter(pollutant == input$pollutant)
    pal <- colorNumeric(palette = colorRampPalette(c("green", "red"))(10),
                        domain = chosen_data$measure)
    leafletProxy("treemap", data = chosen_data) %>% addCircles(lng = chosen_data$long, lat = chosen_data$lat,
                 color = pal(chosen_data$measure), radius = 2, opacity = 0.1)
  })
  
  # Neighborhood
  observeEvent(input$count, {
    if(input$count == "Disable"){
      leafletProxy("treemap")
      #   filtered <- dataSource()%>% filter(spc_common == input$select_treetype)
      #   leafletProxy("mymap", data = filtered) %>%
      #     clearMarkers() %>%
      #     clearControls() %>%
      #     clearShapes() %>%
      #     addMarkers(lng = filtered$longitude,
      #                lat = filtered$latitude, icon = greenLeafIcon)
    }
    else if(input$count == "n"){
      bins <- set_bins(combined@data$n, 4)
      pal <- set_pal(bins)
      legend_labels <- set_labels(bins)
      p1 <- leafletProxy("treemap")  %>%
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
      p1 <- leafletProxy("treemap") %>%
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
  
  
  
  
  
  # Analytics Tab

  output$boro_count_plot<- renderPlot({
    df = data()
    df %>% group_by(borough)%>%
      summarise(rate = n()/nrow(df))%>%
      arrange(rate)%>%
      ggplot(., aes(x= reorder(borough, rate), y=rate)) +
      geom_bar(stat='identity') + 
      ylab("trees count by regions") + xlab ("") + 
      ggtitle("Which regions has most trees?") + 
      theme_minimal() + 
      scale_x_discrete(labels = function(labels) {
        sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))
      })
  })
  
  
  output$boro_size_plot <- renderPlot({
    df = data()
    df %>% group_by(borough)%>%
      summarise(avg_dbh = mean(tree_dbh, na.rm = TRUE))%>%
      arrange(avg_dbh) %>%
      ggplot(., aes(x= reorder(borough, avg_dbh), y=avg_dbh)) +
      geom_bar(stat='identity', fill="chartreuse4") +
      ylab("Avg of Tree Diameter Measured") + xlab ("Boroughs") +
      ggtitle("Largest tree size by region") +
      theme_minimal() +
      theme(text = element_text(size=15), plot.title = element_text(size=20, hjust = 0.5))  + 
      scale_x_discrete(labels = function(labels) {
        sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))
      })
  })
  
  output$health_plot <- renderPlot({
    df = data()
    df = df[df$health != "",]
    df %>%
      group_by(health)%>%
      summarise(rate = n()/nrow(df))%>%
      ggplot(., aes(x= reorder(health, rate), y=rate)) +
      geom_bar(stat='identity', fill="chartreuse4") + 
      ylab("Tree count percentage") + xlab ("Boroughs") + 
      ggtitle("Alive tree by tree type") + 
      theme_minimal() + 
      theme(text = element_text(size=15), plot.title = element_text(size=20, hjust = 0.5)) + 
      scale_x_discrete(labels = function(labels) {
        sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))
      })
  })
  
  output$alive_tree_plot <- renderPlot({
    df = data()
    df[df$status == "Alive",] %>% 
      group_by(spc_common)%>% 
      summarise(rate = n()/nrow(processed_df))%>%
      arrange(desc(rate))%>%
      slice(1:10) %>%
      ggplot(., aes(x= reorder(spc_common, rate), y=rate)) +
      geom_bar(stat='identity', fill="chartreuse4") + 
      ylab("Tree count percentage") + xlab ("Boroughs") + 
      ggtitle("Alive tree by tree type") + 
      theme_minimal() + 
      theme(text = element_text(size=15), plot.title = element_text(size=20, hjust = 0.5)) +
      scale_x_discrete(labels = function(labels) {
        sapply(seq_along(labels), function(i) paste0(ifelse(i %% 2 == 0, '', '\n'), labels[i]))
      })+
      coord_flip()
  })

  observeEvent(input$reset, {
    leafletProxy("treemap") %>%
      clearMarkers() %>%
      clearControls() %>%
      removeLayersControl() %>%
      clearShapes()
  })
  
}
