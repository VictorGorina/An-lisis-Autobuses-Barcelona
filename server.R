# server.R

library(leaflet)
library(ggmap)

shinyServer(function(input, output) {
  
  output$mapa <- renderLeaflet({ 
    ## Crear mapa con las paradas.

   if (input$MAP == 1){
    
     m <- leaflet() %>%
      addTiles() %>%
      addCircleMarkers(lng=stops_bus$stop_lon, lat=stops_bus$stop_lat, radius = stops_bus$numlins * 2.5,
                       popup= as.character(stops_bus$stop_name), popupOptions(zoomAnimation = T, closeOnClick = T))
   }
   
  if (input$MAP == 2) {
    
    Parada_kmeans <- group_by(Busos, PARADA) %>% 
      summarise(median_lag = median(lag, na.rm=TRUE),
                median_tempsparada = median(temps_parades, na.rm=TRUE), numlins = mean(numlins))
    Parada_kmeans <-na.omit(Parada_kmeans)
    Parada_kmeans$median_tempsparada <- as.numeric(Parada_kmeans$median_tempsparada)
    Parada_kmeans <- data.frame(parades = as.character(Parada_kmeans$PARADA), apply(Parada_kmeans[2:4], 2, scale))
    
    km1 <- kmeans(x = Parada_kmeans[,2:4], centers = input$NumClusters, nstart = 50)
    
    Parada_kmeans$cluster <- km1$cluster
    
    stops_cluster <- merge(stops_bus, Parada_kmeans, by.x="stop_id", by.y = "parades")
    
    
    # BCN_MAP <- get_map(location = c(mean(stops_cluster$stop_lon), 
    #                                 mean(stops_cluster$stop_lat)),
    #                    maptype = "terrain", ## Variants roadmap, terrain, satellite, hybrid
    #                    zoom = 13)
    
    pal <- colorFactor(topo.colors(input$NumClusters), domain = stops_cluster$cluster)
    print(str(stops_cluster))
    m <- leaflet() %>%
      addTiles() %>%
      addCircleMarkers(lng=stops_cluster$stop_lon, lat=stops_cluster$stop_lat, color = pal(stops_cluster$cluster))
    
    
    # m <- ggmap(BCN_MAP)+geom_point(data=stops_cluster, alpha = 0.5,
    #                             aes(x=stop_lon, y=stop_lat,
    #                                 col = as.factor(cluster)))
  }
    m  # Print the map
  }
  )
  
})