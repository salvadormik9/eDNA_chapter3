#### Load necessary packages ####

library(rerddap)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(viridis)
library(terra)
library(dplyr)
library(akima)
library(readxl)
library(janitor)
library(ggnewscale)

#### Read in Sampling Sites ####
pew_sites <- read_excel("inputs/pewresearchfellowship_samplingsites.xlsx") %>%
    clean_names()

#### Define Variables, Extract Data, and Wrangle ####
#Set bounding box for SST data 
dl_lon_min <- 122; dl_lon_max <- 125
dl_lat_min <- 8.9; dl_lat_max <- 11.5

# Define Visayas bounding box of latitude and longitude
lon_min <- 122.25; lon_max <- 123.75
lat_min <- 9;   lat_max <- 11.1

# Use NOAA's CoralTemp or OISST dataset
sst_info <- info("erdMH1sstdmday")  # Monthly daytime SST

# Define latitude, longitude, timeframe, and SST data into one dataframe 
sst_data <- griddap(
    sst_info,
    time    = c("2019-01-01T00:00:00Z", "2019-10-16T00:00:00Z"),
    latitude  = c(dl_lat_min, dl_lat_max),
    longitude = c(dl_lon_min, dl_lon_max),
    fields    = "sst"
)

df <- sst_data$data

# Remove NA values (land masking)
df <- df[!is.na(df$sst), ]

# Average over multiple time steps
df_mean <- df %>%
    group_by(longitude, 
             latitude) %>%
    summarise(sst = mean(sst, 
                         na.rm = TRUE), 
              .groups = "drop")

# Get Philippines coastline
ph <- ne_countries(country = "Philippines", 
                   scale = "medium",
                   returnclass = "sf")

# Get province polygons: this is to draw the province lines
ph_prov <- ne_states(country = "Philippines", 
                     returnclass = "sf")


occ <- ph_prov %>% 
    filter(name %in% c("Negros Occidental",
           "Bacolod")) %>% 
    st_union() %>%
    st_sf(geometry = .)

ori <- ph_prov %>% filter(name == "Negros Oriental")%>% 
    st_union()

# The shared border = the part of Occidental's outline that lies within
# ~100 m of Oriental. The small buffer handles vertices that don't line up exactly.
negros_border <- st_intersection(st_boundary(occ), st_buffer(ori, 0.001))

ph_land <- st_union(ph_prov)


#### Plot Maps with Sea Surface Temperature ####
# Plot
ggplot() +
    geom_tile(data = df_mean, 
              aes(x = longitude, 
                  y = latitude, 
                  fill = sst)) +
    geom_sf(data = ph, 
            fill = "#fce5cd", 
            color = "black", 
            linewidth = 0.3) +
    scale_fill_viridis_c(
        option   = "turbo",       # or "plasma", "inferno"
        name     = "SST (°C)",
        na.value = NA
    ) +
    coord_sf(xlim = c(lon_min, 
                      lon_max), 
             ylim = c(lat_min,
                      lat_max)) +
    labs(
        title    = "Map of Sea Surface Temperature in 2019",
        x = "Longitude", 
        y = "Latitude"
    ) +
    theme_minimal()

# Make smoother graph of SST using the akima R package

# ERDDAP data often has slightly irregular spacing between coordinates,
# which prevents direct conversion to a raster. The interp() function
# from the akima package fits a surface through the scattered points
# and evaluates it on a clean, evenly-spaced grid.

# Concept of Triangulation Interpolation:

# Delaunay triangulation takes an irregular scatter of SST points and
# connects them into a mesh of triangles (Delaunay triangulation), where each 
# triangle is defined by 3 nearby data points. 
# For any new grid point, triangulation interpolation finds which triangle 
# it falls inside and linearly interpolates between the 3 triangle corners.

# Interpolate to a regular grid
interp_result <- with(df_mean, 
                      interp(
                          x    = longitude, 
                          y    = latitude, 
                          z    = sst, # the values to interpolate (sea surface temperature)
                          xo   = seq(121, 126, by = 0.05),   # output lon grid: 121°E to 126°E every 0.05°
                          yo   = seq(9,   13,  by = 0.05),  # output lat grid: 9°N to 13°N every 0.05°
                          # 0.05° ≈ ~5.5 km resolution — decrease for finer detail, increase for speed
                          linear = TRUE,
                          # use linear (triangular) interpolation; 
                          # set FALSE for smoother spline (slower)
                          duplicate = "mean"
                          # if two points share the same location,
                          # use their average instead of throwing an error
                      )
)

# interp() returns a list with $x (longitudes), $y (latitudes), and $z (matrix
# of SST values). We need to "unfold" these into a flat x/y/z table that
# ggplot can understand
df_regular <- expand.grid(
    longitude = interp_result$x,
    latitude  = interp_result$y
)

# Flatten the SST matrix into a single column matching the row order of df_regular
df_regular$sst <- as.vector(interp_result$z)
df_regular <- df_regular[!is.na(df_regular$sst), ]

# Let's plot our data! 
# geom_raster() is a fast version of geom_tile() for evenly-spaced grids.
# interpolate = TRUE (geom_raster): visual smoothing that smooths the appearance of the pixels on 
# screen when ggplot renders the image. 

island_fill <- c("Negros Occidental" = "#EDAFB8",
                  "Negros Oriental" = "#BEE1C0"
                  )

ggplot() +
    geom_raster(data = df_regular, 
                aes(x = longitude, 
                    y = latitude, 
                    fill = sst),
                interpolate = TRUE) + 
    scale_fill_viridis_c(option = "turbo", 
                         name = "SST (°C)",
                         na.value = NA) +
    geom_sf(data = ph_land, 
            fill = "#EDEDD9", 
            color = "black", 
            linewidth = 0.3) +
    geom_sf(data = occ,
            fill = "#F8E1E5",
            color = "black",
            linewidth = 0.5) +
    geom_sf(data = ori,
            fill = "#CFEBD0",
            color = "black",
            linewidth = 0.5) +
    geom_sf(data = negros_border,
            color = "black",
            linewidth = 0.6) +
    new_scale_fill()+ 
    geom_point(data = pew_sites,
               aes(x = longitude,
                   y = latitude,
                   fill = island_side,
                   shape = island_side),
               color = "black",
               size = 3,
               stroke = 0.8) +
    scale_fill_manual(values = island_fill) +
    scale_shape_manual(values = c("Negros Occidental" = 21, 
                                  "Negros Oriental" = 24)) +
    coord_sf(xlim = c(lon_min, 
                      lon_max), 
             ylim = c(lat_min,
                      lat_max),
             expand = FALSE) +
    theme_bw() +
    labs(x = "Longitude",
         y = "Latitude",
         fill = "Province",
         shape = "Province")

ggsave("outputs/pewsamplingsites_map.png",
       device = "png",
       units = "in",
       width = 9,
       height = 8)

