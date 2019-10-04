source("../global.R")
          
######## Server

server <- function(input, output){
    ########## map1
    output$map1 <- renderLeaflet({
        if(input$count == "n"){
            bins <- set_bins(combined@data$n, 4)
            pal <- set_pal(bins)
            legend_labels <- set_labels(bins)
            p1 <- leaflet(options = leafletOptions(minZoom = 10)) %>%
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
            p1 <- leaflet(options = leafletOptions(minZoom = 10)) %>%
                addProviderTiles("CartoDB.Positron") %>%
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
        
        