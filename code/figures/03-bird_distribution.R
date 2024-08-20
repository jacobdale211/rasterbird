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
  geom_sf(data = brange[27,], fill = "lightblue", color = NA) +
  theme(panel.background = element_rect(fill = "white")) +
  ggtitle("Tundra Swan Wintering Distributions")
print(fp2)

# lazy figures, wrote in each stressor one at a time
test <- stars::read_stars("data/data-format/bird-grid/bird-grid_White-rumped_Sandpiper.tif")
test <- sf::st_as_sf(test)

# whole range -- Red Knot + study area
ggplot() +
  geom_sf(data = std_ar, fill = "grey60", color = "black") +
  geom_sf(data = test, fill = alpha("lightblue", 1.0), color = NA) +
  theme(panel.background = element_rect(fill = "white")) +
  labs(title = "White-rumped Sandpiper Presence/Absence")


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


# library(tmap)
# tm_shape(std_ar[7]) +
#   tm_fill(col = "grey60") +
#   tm_shape(brange[1,]) +
#   tm_fill(col = "lightblue", title = "Wintering Range") +
#   tm_shape(a_f) +
#   tm_fill(col = "orange", alpha = 0.4, title = "Atlantic Flyway") +
#   tm_layout(frame = FALSE)+
#   tm_legend(outside = TRUE, legend.position = c("right", "top"))

