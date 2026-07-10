# ---- Load necessary packages ----

library(rerddap)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(terra)
library(dplyr)
library(akima)

# ---- Define Variables, Extract Data, and Wrangle ----

# Define Visayas bounding box of latitude and longitude
lon_min <- 122; lon_max <- 125
lat_min <- 9;   lat_max <- 11.5

# Use NOAA's CoralTemp or OISST dataset
sst_info <- info("erdMH1sstdmday")  # Monthly daytime SST

# Define latitude, longitude, timeframe, and SST data into one dataframe 
sst_data <- griddap(
    sst_info,
    time    = c("2019-01-01T00:00:00Z", "2019-10-16T00:00:00Z"),
    latitude  = c(lat_min, lat_max),
    longitude = c(lon_min, lon_max),
    fields    = "sst"
)

df <- sst_data$data

# Remove NA values (land masking)
df <- df[!is.na(df$sst), ]

# Average over multiple time steps
df_mean <- df %>%
    group_by(longitude, latitude) %>%
    summarise(sst = mean(sst, na.rm = TRUE), .groups = "drop")

# Get Philippines coastline
ph <- ne_countries(country = "Philippines", scale = "medium", returnclass = "sf")

# ---- Maps of Sea Surface Temperature ----
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

ggplot() +
    geom_raster(data = df_regular, 
                aes(x = longitude, 
                    y = latitude, 
                    fill = sst),
                interpolate = TRUE) + 
    geom_sf(data = ph, 
            fill = "wheat", 
            color = "gray40", 
            linewidth = 0.3) +
    scale_fill_viridis_c(option = "turbo", 
                         name = "SST (°C)",
                         na.value = NA) +
    coord_sf(xlim = c(lon_min, lon_max), 
             ylim = c(lat_min, lat_max),
             expand = FALSE) +
    theme_bw() +
    labs(x = "Longitude",
         y = "Latitude")
    )

