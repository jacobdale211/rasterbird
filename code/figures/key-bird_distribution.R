# Formatting images for raw bird ranges


library(tidyverse)
library(maps)
library(ggplot2)
library(dplyr)
library(ggplot2)
library(rphylopic)
library(sf)

# read in the raw shape file
brange = sf::st_read("./data/data-raw/birdlife/birds_multistress/Bylot_non_breeding_range.shp")
america_poly <- sf::st_read("map.geojson")
 # brange <- sf::st_crop(brange, america_poly)



library(rnaturalearth)
library(rnaturalearthdata)



# # # maps # # #
# all 50 states
america_map <- ne_countries(country = "United States of America", returnclass = "sf")

# south america
south_america_map <- ne_countries(continent = "South America", returnclass = "sf")

# lower 48
b <- ne_states(country = "United States of America")
b <- b[b$name != "Alaska", ]
b <- b[b$name != "Hawaii", ]
plot(b[7])

# study area
world <- ne_countries(scale = "medium", returnclass = "sf")
std_ar <- subset(world, continent %in% c("North America", "South America"))
std_ar <- st_crop(std_ar, america_poly)
plot(std_ar)

# all flyways ***NEED TO RECOVER FLYWAY GEOJSONS***
fly <- sf::st_read("Shorebird_Americas_Flyway_boundaries.geojson")

# atlantic flyway
a_f <- sf::st_read("a_f.gpkg")



# # #plots # # #
# First test!! American Golden Plover, South America
fp <- ggplot() +
  geom_sf(data = south_america_map[1], fill = "grey60", color = NA) +
  geom_sf(data = brange[1,],fill = "lightblue", color = NA)  +
  theme(panel.background = element_rect(fill = "white")) +
  ggtitle("American Golden-Plover Presence/Absence")


print(fp)

# America range -- Tundra Swan !!, North America
fp2 <- ggplot() +
  geom_sf(data = b[7], fill = "grey60", color = NA) +
  geom_sf(data = brange[10,], fill = "lightblue", color = NA) +
  theme(panel.background = element_rect(fill = "white")) +
  ggtitle("Tundra Swan Wintering Distributions")
print(fp2)
 
# ====================================================================================
# lazy figures, you have to write in each bird one at a time
# and use in the script below
test <- stars::read_stars("data/data-format/bird-grid/bird-grid_Long-tailed_Duck.tif")
test <- sf::st_as_sf(test)

# whole range -- Red Knot + study area
ggplot() +
  geom_sf(data = std_ar, fill = "grey60", color = "black") +
  geom_sf(data = test, fill = alpha("lightblue", 1.0), color = NA) +
  theme(panel.background = element_rect(fill = "white")) +
  labs(title = "Long-tailed Duck Presence/Absence")
# ====================================================================================


# all flyways + north america
ggplot() +
  geom_sf(data = std_ar[,169], fill = "grey60", color = "black") +
  geom_sf(data = brange[1,], fill = "lightblue", color = NA) +  
  geom_sf(data = fly[4,], fill = alpha("green", 0.4)) +
  geom_sf(data = fly[5,], fill = alpha("blue", 0.4)) +
  geom_sf(data = fly[6,], fill = alpha("purple", 0.4)) +
  theme(panel.background = element_blank()) +
  theme(legend.title = element_blank())


# atlantic flyway + bird range
ggplot() +
  geom_sf(data = std_ar, fill = "grey60", color = "black") +
  geom_sf(data = a_f, fill = alpha("purple", 0.4), color = "purple") +
  theme(panel.background = element_rect(fill = "white")) +
  labs(title = "Atlantic Flyway")


# all buffered bird ranges in one, with colors for category -----------------------------------------------------------
# Load required libraries
library(terra)
library(ggplot2)
library(sf)
library(tidyverse)

# Define directories
wfras <- "./data/data-format/wf-grid/"
sbras <- "./data/data-format/sb-grid/"  # Adjust this path for your shorebird files

# Get file paths
waterfowl_files <- dir(wfras, pattern = ".tif$", full.names = TRUE)
shorebird_files <- dir(sbras, pattern = ".tif$", full.names = TRUE)

# Function to process a single raster file
process_raster <- function(file_path, type) {
  rast <- rast(file_path)
  poly <- as.polygons(rast) %>%
    st_as_sf() %>%
    mutate(
      type = type,
      filename = basename(file_path)
    )
  return(poly)
}

# Process all files
waterfowl_polygons <- map_dfr(waterfowl_files, ~process_raster(.x, "Waterfowl"))
shorebird_polygons <- map_dfr(shorebird_files, ~process_raster(.x, "Shorebird"))

# Combine all polygons
all_ranges <- bind_rows(waterfowl_polygons, shorebird_polygons)

# Create the plot with clean white background
ggplot() +
  geom_sf(data = all_ranges, aes(fill = type), alpha = 0.7) +
  scale_fill_manual(values = c("Waterfowl" = "#90EE91", "Shorebird" = "#ADD8E6")) +
  theme_void() +  # This removes all background elements
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(title = "Bird Range Distribution") +
  coord_sf()




