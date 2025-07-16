library(osmdata)
library(sf)
library(tidyverse)

# Set location: Bangalore
location <- "raichur, India"
# https://wiki.openstreetmap.org/wiki/Map_features


# Get Roads
roads <- opq(location) %>%
  add_osm_feature(key = "highway", value = "tertiary") %>%
  osmdata_sf() %>%
  `[[`("osm_lines")

# Get Rivers
rivers <- opq(location) %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf() %>%
  `[[`("osm_lines")

# Get Lakes
lakes <- opq(location) %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf() %>%
  `[[`("osm_polygons")


# Plot all features together
ggplot() +
  geom_sf(data = roads, color = "gray40", size = 0.2) +
  # geom_sf(data = rivers, color = "blue", size = 0.5) +
  geom_sf(data = lakes, fill = "lightblue", color = NA) +
  theme_minimal() +
  labs(title = "OpenStreetMap: Raichur", subtitle = "Roads, Rivers & Lakes")



bbox <- c(
  xmin = 77.55,  # min longitude
  ymin = 12.90,  # min latitude
  xmax = 77.65,  # max longitude
  ymax = 13.05   # max latitude
)

roads <- opq(bbox = bbox) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf() %>%
  `[[`("osm_lines")

# If you have a shapefile or GeoJSON boundary, you can also use a polygon:
# my_boundary <- st_read("path_to_your_boundary.shp")  # or GeoJSON
# bbox <- st_bbox(my_boundary)





