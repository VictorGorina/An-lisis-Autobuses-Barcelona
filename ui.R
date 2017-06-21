#install.packages(c("leaflet","shiny"))

library(leaflet)
library(shiny)

shinyUI(fluidPage( 
  sidebarLayout(
    sidebarPanel(
      radioButtons("MAP", label = h3("Select map to show:"),
                   choices = list("Lag Affinity" = 1, "Cluster" = 2), 
                   selected = 1),
      numericInput("NumClusters", label = h3("Choose number of clusters:"), value = 8)
    ),
    
    mainPanel(leafletOutput("mapa", height = 700))
  )
))
