# Multipolygon to polygon
library(stars)
library(sf)

data_dir <- "data/data-format/bird-grid"
files <- list.files(data_dir, pattern = ".tif", full.names = TRUE)


# Please note, the numbers within p() do not align properly with the number of species
# included in the final list. check the p() object to ensure the correct species correlates
# with the correct object
p <- list()
for (file in files) {
  obj <- stars::read_stars(file)
  polygon <- sf::st_as_sf(obj)
  p[[file]] <- polygon
}



#--------------------------------------------------------------------
# extracting values for final table
library(dplyr)
df <- read.csv("csvs/sumsts_noshpg.csv")

df <- df %>%
  mutate(sp = gsub("_", " ", sp))

result <- df %>%
  group_by(sp) %>%
  summarize(total_sd = sum(sd, na.rm = TRUE)) %>%
  arrange(desc(total_sd))
#--------------------------------------------------------------------










# (Fine for the moment because I know the orientation of the files, but I will change it in the future)
# Remove 2 (Bairds, no pop trends)
# p <- p[-2]
# Remove 6 (Common-ringed Plover, not in study area)
 # p <- p[-6]


# Birds that need to have separated ranges:
# 2, 4, 6, 7, 8, 9, 10, 11*, 13, 14, 16

# # # Individualizing bird ranges # # #

# Note: buffer of 2 degrees applied already in bird grid.. no need to reapply
# for restructuring of ranges 



# Function to add species column for Philippe's method
# add_species_column <- function(sfg_object, species_vector) {
#   coords <- st_coordinates(sfg_object)
#   
#   new_df <- data.frame(x = coords[, 1], y = coords[, 2], species = species_vector)
#   
#   new_sfg_object <- st_as_sf(new_df, coords = c("x", "y"), crs = st_crs(sfg_object))
#   return(new_sfg_object)
# }



# 1, American Golden-Plover, 1 range
amgplo_u <- sf::st_union(p[[1]])
amgplo_d <- sf::st_cast(amgplo_u, "POLYGON")

st_area(amgplo_d)

#READDED### 2 Bairds Sandpiper
baisan_u <- sf::st_union(p[[2]])
baisan_d <- sf::st_cast(baisan_u, "POLYGON")

# 3, Black-bellied_Plover
blbplo_u <- sf::st_union(p[[3]])
blbplo_d <- sf::st_cast(blbplo_u, "POLYGON")


# 4, Buff-breasted Sandpiper, 1 range
bubsan_u <- sf::st_union(p[[4]])
bubsan_d <- sf::st_cast(bubsan_u, "POLYGON")

# 5, Cackling Goose
cacgoo_u <- sf::st_union(p[[5]])
cacgoo_d <- sf::st_cast(cacgoo_u, "POLYGON")


# # # 6, Glaucous Gull
#  glagul_u <- sf::st_union(p[[6]])
#  glagul_d <- sf::st_cast(glagul_u, "POLYGON")


# 6, King Eider
kineid_u <- sf::st_union(p[[8]])
kineid_d <- sf::st_cast(kineid_u, "POLYGON")


# 7, Long-tailed Duck
lotduc_u <- sf::st_union(p[[9]])
lotduc_d <- sf::st_cast(lotduc_u, "POLYGON")

# # 9 Pacific Loon
# pacloo_u <- sf::st_union(p[[9]])
# pacloo_d <- sf::st_cast(pacloo_u, "POLYGON")

# # # 10, Long-tailed Jaeger
# lotjae_u <- sf::st_union(p[[10]])
# lotjae_d <- sf::st_cast(lotjae_u, "POLYGON")
# # 
# # # 11, Parasitic Jaeger
# parjae_u <- sf::st_union(p[[11]])
# parjae_d <- sf::st_cast(parjae_u, "POLYGON")




# 8, Pectoral Sandpiper
pecsan_u <- sf::st_union(p[[13]])
pecsan_d <- sf::st_cast(pecsan_u, "POLYGON")




# 9, Red Knot * need to remove EU and Africa populations
redkno_u <- sf::st_union(p[[14]])
redkno_d <- sf::st_cast(redkno_u, "POLYGON")
# indices <- c(1,4,8)
# redkno_d <- redkno_d[indices]

# Removing EU and Africa polygons
# removed because America crop solves this issue
# redkno_d <- redkno_d[-3] # Run 3 times


# 10, Red-throated Loon, 1 range
retloo_u <- sf::st_union(p[[15]])
retloo_d <- sf::st_cast(retloo_u, "POLYGON")



# 11, Ruddy Turnstone * need to remove EU and Africa populations
rudtur_u <- sf::st_union(p[[16]])
rudtur_d <- sf::st_cast(rudtur_u, "POLYGON")
# indices <- c(10)
# rudtur_d <- rudtur_d[indices]



# 12, Snow Goose
snogoo_u <- sf::st_union(p[[17]])
snogoo_d <- sf::st_cast(snogoo_u, "POLYGON")


# 13, Tundra Swan, 1 range
tunswa_u <- sf::st_union(p[[18]])
tunswa_d <- sf::st_cast(tunswa_u, "POLYGON")


# 14, White-rumped Sandpiper
whrsan_u <- sf::st_union(p[[19]])
whrsan_d <- sf::st_cast(whrsan_u, "POLYGON")

# For jacob script
birds <- list(
  amgplo_d,
  baisan_d,
  blbplo_d,
  bubsan_d,
  cacgoo_d,
  kineid_d,
  lotduc_d,
  pecsan_d,
  redkno_d,
  retloo_d,
  rudtur_d,
  snogoo_d,
  tunswa_d,
  whrsan_d) |>
  lapply(sf::st_as_sf) |>
  dplyr::bind_rows()

america_poly <- sf::st_read("america.geojson")

# library(ggplot2)
# ggplot() +
#   geom_sf(data = birds, fill = "blue", color = "black") +
#   geom_sf(data = america_poly, fill = "red", color = "black") +
#   labs(title = "Polygons1 and Polygons2 on Top of Each Other") +
#   theme_minimal()
# 




#BEFORE americas crop!!! (EU ranges are present)
st_write(birds, "birds_upd.gpkg", driver = "GPKG", append=FALSE)

# shp <- st_read(
#   "birds.gpkg")
# america_poly <- st_read("america.geojson")
# shp <- st_crop(shp, america_poly)



