#### Load necessary packages ####
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(terra)
library(dplyr)
library(readxl)
library(janitor)
library(ggnewscale)

#### Download NOAA CoralTemp data (Coral Reef Watch) ####
# Only downloads if the file isn't already in the inputs folder
options(timeout = 3600)  # default 60 s timeout is too short for big files

dir.create("inputs/coral_reef_watch", recursive = TRUE, showWarnings = FALSE)

sst_file <- "inputs/coral_reef_watch/ct5km_climatology_v3.1.nc"

# NOAA Coral Reef Watch 5 km climatology (global file, ~78 MB)
if (!file.exists(sst_file)) {
    download.file(
        url = "https://www.star.nesdis.noaa.gov/pub/sod/mecb/crw/data/5km/v3.1_op/climatology/nc/ct5km_climatology_v3.1.nc",
        destfile = sst_file,
        mode = "wb"   # "wb" is essential on Windows or the file gets corrupted
    )
}

#### Read in Sampling Sites ####
pew_sites <- read_excel("inputs/pewresearchfellowship_samplingsites.xlsx") %>%
    clean_names()

#### Define map window ####
# Visayas bounding box of latitude and longitude (plot window)
lon_min <- 122.25; lon_max <- 123.75
lat_min <- 9;      lat_max <- 11.1

# Slightly larger extent used to crop the SST raster (so the plot edges
# are fully covered)
my_ext <- ext(121, 126, 8.5, 12)

#### Read and wrangle CoralTemp SST (Coral Reef Watch max monthly mean) ####
sst_rast <- rast(sst_file,
                 subds = "sst_clim_mmm")

sst_rast <- sst_rast[[1]]
sst_rast <- crop(sst_rast, my_ext)

# terra raster -> data frame that ggplot can use
# (na.rm = TRUE drops land cells, which are masked in the CoralTemp data)
df_regular <- as.data.frame(sst_rast,
                            xy = TRUE,
                            na.rm = TRUE)
names(df_regular) <- c("longitude", "latitude", "sst")

sst_lims <- range(df_regular$sst) 
sst_top  <- 29.8

#### Get map polygons ####
# Province polygons: this is to draw the province lines
ph_prov <- ne_states(country = "Philippines",
                     returnclass = "sf")

occ <- ph_prov %>%
    filter(name %in% c("Negros Occidental",
                       "Bacolod")) %>%
    st_union() %>%
    st_sf(geometry = .)

ori <- ph_prov %>%
    filter(name == "Negros Oriental") %>%
    st_union()

# The shared border = the part of Occidental's outline that lies within
# ~100 m of Oriental. The small buffer handles vertices that don't line up exactly.
negros_border <- st_intersection(st_boundary(occ), st_buffer(ori, 0.001))

ph_land <- st_union(ph_prov)

#### Plot map with CoralTemp SST ####
# CoralTemp is already on a regular 0.05 degree (~5 km) grid, so no akima
# interpolation is needed. geom_raster() works directly, and
# interpolate = TRUE smooths how the pixels appear on screen.

island_fill <- c("Negros Occidental" = "#EDAFB8",
                 "Negros Oriental"   = "#BEE1C0")

# Use the same SST palette as the regional sampling map
sst_pal <- c("#0407C8", 
             "#8ECBFA",
             "#ECFDAE", 
             "#ED772F",
             "#83180F")

sst_vals <- scales::rescale(seq(sst_lims[1], sst_lims[2], length.out = length(sst_pal)),
                            from = c(sst_lims[1], sst_top))

p <- ggplot() +
    geom_raster(data = df_regular,
                aes(x = longitude,
                    y = latitude,
                    fill = sst),
                interpolate = TRUE) +
    scale_fill_gradientn(colours = c(sst_pal, tail(sst_pal, 1)),
                         values  = c(sst_vals, 1),
                         name    = "Maximum Monthly\nMean SST (°C)",
                         limits  = c(sst_lims[1], sst_top),
                         breaks  = seq(29, 29.8, by = 0.2),
                         labels  = c("29", "29.2", "29.4", "29.6", "29.8"),
                         na.value = NA,
                         guide   = guide_colourbar(order = 2)) +
    # To use the viridis turbo palette instead, swap the line above for:
    # scale_fill_viridis_c(option = "turbo", name = "Maximum Monthly\nMean SST (°C)", na.value = NA) +
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
    new_scale_fill() +
    geom_point(data = pew_sites,
               aes(x = longitude,
                   y = latitude,
                   fill = island_side,
                   shape = island_side),
               color = "black",
               size = 3,
               stroke = 0.8) +
    scale_fill_manual(values = island_fill,
                      guide = guide_legend(order = 1)) +
    scale_shape_manual(values = c("Negros Occidental" = 21,
                                  "Negros Oriental"   = 24),
                       guide = guide_legend(order = 1)) +
    coord_sf(xlim = c(lon_min, lon_max),
             ylim = c(lat_min, lat_max),
             expand = FALSE) +
    theme_bw() +
    labs(x = "Longitude",
         y = "Latitude",
         fill = "Province",
         shape = "Province")

p

ggsave("outputs/pewsamplingsites_map.png",
       plot = p,
       device = "png",
       units = "in",
       width = 9,
       height = 8,
       dpi = 300)
