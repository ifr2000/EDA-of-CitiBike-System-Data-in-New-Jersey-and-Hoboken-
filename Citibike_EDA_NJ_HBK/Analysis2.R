# Get list of csv files in directory
file_list <- list.files(pattern = "*.csv")
Citibike <- NULL

for (i in 1:length(file_list)) {
  # Read csv file into temporary dataframe
  temp_df <- read.csv(file_list[i])
  if (i == 1) {
   Citibike <- temp_df
  } else {
    # Remove header and append to final dataframe
    temp_df <- temp_df[-1, ]
    Citibike <- rbind(Citibike, temp_df)
  }
}

Citibike

library(tidyverse)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(scales)

top_stations <- Citibike %>%
  group_by(start_station_name) %>%
  summarise(trip_count = n()) %>%
  arrange(desc(trip_count))

station_locations <- Citibike %>%
  distinct(start_station_name, start_lat, start_lng) %>%
  left_join(top_stations, by = "start_station_name")%>%
  setNames(c("start_station_name", "lat", "lng","trip_count"))

# Define a color scale based on the trip count
colorScale <- colorNumeric(palette = "Reds", domain = c(0, max(top_stations$trip_count)))

leaflet(station_locations) %>%
  addTiles() %>%
  setView(lng = -73.9772, lat = 40.7488, zoom = 12) %>%
  addCircleMarkers(
    radius = 5,
    color = ~colorScale(trip_count),
    stroke = FALSE,
    fillOpacity = 0.8,
    label = ~start_station_name,
    popup = ~paste(start_station_name, "<br>", "Trip Count:", trip_count),
    group = "All Stations"
  ) %>%
  addLayersControl(
    overlayGroups = "All Stations",
    options = layersControlOptions(collapsed = FALSE)
  ) 



#Casual stations:
top_casual <- Citibike %>%
  filter(member_casual == "casual") %>%
  group_by(start_station_name) %>%
  summarise(trip_count = n()) %>%
  arrange(desc(trip_count)) %>%
  slice_head(n = 25)

station_locations_casual <- Citibike %>%
  filter(member_casual == "casual") %>%
  distinct(start_station_name, start_lat, start_lng) %>%
  filter(start_station_name %in% top_casual$start_station_name) %>%
  left_join(top_casual, by = "start_station_name") %>%
  setNames(c("start_station_name", "lat", "lng", "trip_count"))

colorScale_casual <- colorNumeric(palette = "Greens", domain = c(0, max(top_casual$trip_count)))

casual<-leaflet(station_locations_casual) %>%
  addTiles() %>%
  setView(lng = -73.9772, lat = 40.7488, zoom = 12) %>%
  addCircleMarkers(
    radius = 5,
    color = ~colorScale_casual(trip_count),
    stroke = FALSE,
    fillOpacity = 0.8,
    label = ~start_station_name,
    popup = ~paste(start_station_name, "<br>", "Trip Count:", trip_count),
    group = "Top 25 Casual Stations"
  ) %>%
  addLayersControl(
    overlayGroups = c("Top 25 Casual Stations", "Top 25 Member Stations"),
    options = layersControlOptions(collapsed = FALSE)
  )


top_member <- Citibike %>%
  filter(member_casual == "member") %>%
  group_by(start_station_name) %>%
  summarise(trip_count = n()) %>%
  arrange(desc(trip_count)) %>%
  slice_head(n = 25)

station_locations_member <- Citibike %>%
  filter(member_casual == "member") %>%
  distinct(start_station_name, start_lat, start_lng) %>%
  filter(start_station_name %in% top_member$start_station_name) %>%
  left_join(top_member, by = "start_station_name") %>%
  setNames(c("start_station_name", "lat", "lng", "trip_count"))

colorScale_member <- colorNumeric(palette = "Oranges", domain = c(0, max(top_member$trip_count)))

member<-leaflet(station_locations_member) %>%
  addTiles() %>%
  setView(lng = -73.9772, lat = 40.7488, zoom = 12) %>%
  addCircleMarkers(
    radius = 5,
    color = ~colorScale_member(trip_count),
    stroke = FALSE,
    fillOpacity = 0.8,
    label = ~start_station_name,
    popup = ~paste(start_station_name, "<br>", "Trip Count:", trip_count),
    group = "Top 25 Member Stations"
  ) %>%
  addLayersControl(
    overlayGroups = c("Top 25 Casual Stations", "Top 25 Member Stations"),
    options = layersControlOptions(collapsed = FALSE)
  )

casual #TO VIEW TOP 25 STATIONS USED BY CASUAL RIDERS

member # TO VIEW TOP 25 STATIONS USED BY MEMBER RIDERS.



