library(dplyr)
library(dbscan)
library(cluster) 
library(leaflet)  
library(sp)

setwd("/Users/illurisaisandeep/Downloads")
df <- read.csv("ais.csv", header = TRUE)

df$t <- as.POSIXct(df$t, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

df <- df %>%
  filter(complete.cases(.))

df <- df %>%
  group_by(shipid) %>%
  filter(t == max(t)) %>%
  ungroup()

df$lon <- as.numeric(df$lon)
df$lat <- as.numeric(df$lat)

data_clustering <- df %>%
  select(lon, lat)

dbscan_result <- dbscan(data_clustering, eps = 0.05, minPts = 5)

df$cluster <- dbscan_result$cluster

get_convex_hull <- function(cluster_data) {
  coords <- as.matrix(cluster_data[, c("lon", "lat")])
  hull_indices <- chull(coords) 
  hull_coords <- coords[hull_indices, ] 
  return(hull_coords)
}

color_palette <- colorFactor(palette = "Set1", domain = df$cluster)

m <- leaflet(df) %>%
  addTiles()

unique_clusters <- unique(df$cluster[df$cluster > 0]) 
for (cluster in unique_clusters) {
  cluster_data <- df %>% filter(cluster == !!cluster)
  hull_coords <- get_convex_hull(cluster_data)  
  
  m <- m %>% addPolygons(
    lng = hull_coords[, 1], 
    lat = hull_coords[, 2], 
    color = color_palette(cluster), 
    weight = 5, 
    fill = FALSE, 
    popup = paste("Cluster:", cluster)
  )
}

m <- m %>%
  addCircleMarkers(
    lng = ~lon, lat = ~lat, 
    color = ~color_palette(cluster), 
    fillColor = ~color_palette(cluster), 
    radius = 5, stroke = FALSE, fillOpacity = 1, 
    popup = ~paste("Ship ID:", shipid)
  )

m <- m %>%
  addLegend(
    position = "bottomright", 
    pal = color_palette, 
    values = ~cluster, 
    title = "Clusters", 
    opacity = 1
  )

m <- m %>% setView(lng = mean(df$lon), lat = mean(df$lat), zoom = 10)

m