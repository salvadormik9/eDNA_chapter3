# Install packages if needed
install.packages(c("osmdata", "sf", "ggplot2"))

library(osmdata)
library(sf)
library(ggplot2)

# Query Apo Island from OpenStreetMap
apo_query <- opq(bbox = c(123.155, 9.065, 123.195, 9.105)) |>
  add_osm_feature(key = "place", value = "island")

apo_island <- osmdata_sf(apo_query)

# Extract the polygon
island_poly <- apo_island$osm_polygons

# Plot with customizable fill color — change "fill" to any color you like
ggplot(island_poly) +
  geom_sf(fill = "#8FBC8F", color = "black", linewidth = 0.5) +
  theme_minimal() +
  labs(title = "Apo Island, Philippines") +
  theme(panel.background = element_rect(fill = "#a8d8ea"))  # ocean color