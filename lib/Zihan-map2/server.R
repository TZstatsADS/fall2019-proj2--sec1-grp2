source("global.R")

popup1 <- paste("<strong>Neighborhood: </strong>", data_by_borough[[i]]@data$ntaname,
                "<br><strong>Counts: </strong>", data_by_borough[[i]]@data$n)

server <- function(input, output){
    greenLeafIcon <- makeIcon(
        iconUrl = "fig/tree.png",
        iconWidth = 18, iconHeight = 20,
        iconAnchorX = 22, iconAnchorY = 94
    )
    
    output$map2 <- renderLeaflet({
        leaflet(options = leafletOptions(minZoom = 10)) %>%
            addProviderTiles("CartoDB.Positron") %>%
            setView(lat = NYC_coord["lat"], lng = NYC_coord["lon"], zoom = 10) %>%
            addPolygons(data = combined, color = "black", weight = 1, 
                        fillOpacity = 0)
    })
    
    proxy <- leafletProxy("map2")
    
    observeEvent(input$borough,{
        b <- input$borough
        
        if(b == "All"){
            popup <- paste("<strong>Type: </strong>", tree$spc_common,
                           "<br><strong>Status: </strong>", tree$status,
                           "<br><strong>Address: </strong>", tree$address)
            
            proxy %>%
                clearMarkers() %>%
                clearShapes() %>%
                flyTo(lat = NYC_coord["lat"], lng = NYC_coord["lon"], zoom = 10) %>%
                addPolygons(data = combined, color = "black", weight = 1, 
                            fillOpacity = 0) %>%
                addMarkers(lng = tree$longitude, lat = tree$latitude, popup = popup)
                           
        }
        
        else{
            filtered <- tree %>%
                filter(borough == b)
            
            popup <- paste("<strong>Type: </strong>", filtered$spc_common,
                           "<br><strong>Status: </strong>", filtered$status,
                           "<br><strong>Address: </strong>", filtered$address)
            
            centroid <- gCentroid(combined[combined@data$boro_name == b, ])
            
            proxy %>%
                clearMarkers() %>%
                clearShapes() %>%
                flyTo(lng = centroid@coords[1], lat = centroid@coords[2], zoom = 11) %>%
                addPolygons(data = combined, color = "black", weight = 1, 
                            fillOpacity = 0) %>%
                addMarkers(lng = filtered$longitude, lat = filtered$latitude,
                           popup = popup) %>%
                addPolygons(data = combined[combined@data$boro_name == b, ],
                            color = "lightblue", weight = 3,
                            fillColor = "lightgreen", fillOpacity = 0.2)
        }
        
    })
}
