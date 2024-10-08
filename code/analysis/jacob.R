library(terra)
library(sf)
library(exactextractr)
library(lwgeom)


sf_use_s2(FALSE)

america_poly <- st_read("america.geojson")

### NOTE BIRDS_UPD WAS OVERWRITTEN MUST UPDATE
### updated (without marine species)
shp <- st_read(
  "birds_upd.gpkg")

#shp <- st_crop(shp, america_poly)

 shp <- st_crop(shp, america_poly)


# Add bird range areas
shp <- shp %>%
  dplyr::mutate(range_size = st_area(geom))

# Getting species and range names
species <- c()
# this list might have to be changed manually when bird ranges are updated
list_num_birds <- c(1,5,7,1,3,1,10,2,3,1,1,2,2,4)
list_name_birds <- c("American_Golden-Plover", "Bairds_Sandpiper","Black-bellied_Plover", "Buff-breasted_Sandpiper", "Cackling_Goose",
                     "King_Eider", "Long-tailed_Duck", "Pectoral_Sandpiper",  "Red_Knot", "Red-throated_Loon", "Ruddy_Turnstone",
                     "Snow_Goose", "Tundra_Swan", "White-rumped_Sandpiper")
# removed marine birds
# "Glaucous_Gull",
# "Long-tailed_Jaeger", "Pacific_Loon","Parasitic_Jaeger",


# Loop over each bird name and number
for (i in seq_along(list_name_birds)) {
  bird_name <- list_name_birds[i]
  bird_num <- list_num_birds[i]
  
# Generate names with corresponding numbers and append to the vector
  for (j in 1:bird_num) {
    species <- c(species, paste(bird_name, j, sep = " "))
  }
}

shp$species <- species

# Function
extract_bird_data <- function(raster_file, shp) {
  r <- rast(raster_file)
  plot(shp)

  res <- exact_extract(r, shp, include_cols = c("species","range_size"), include_xy = TRUE)

  res <- do.call(rbind, res)
}


# List of all drivers 
files <- list.files("data/data-stressors", full.names = TRUE)

res <- lapply(files, extract_bird_data, shp = shp)
write.csv(res, "res_terra_moresp.csv")



### formatting dataframe



res <- read.csv("csvs/res_terra_moresp.csv")

# Clean up new format
library(dplyr)
# Rename stressors
res <- res %>%
  rename(
    inorganic = value,
    invasives = value.1,
    lights_halp = value.2,
    ocn_pol = value.3,
    plm_fert = value.4,
    plm_pest = value.5,
    pop_halp = value.6,
    built = value.7,
    croplands = value.8,
    lights_vent = value.9,
    navwater = value.10,
    pastures  = value.11,
    pop_vent = value.12,
    railways = value.13,
    roads = value.14
  )

# Seperate species from range number
res <- tidyr::separate(res, species, into = c("sp", "range"), sep = " ")

# Remove extra columns
s_t_r <- c("range_size.", "coverage_fraction.", "x.", "y.", "species.")
res <- res %>%
  select(-contains(s_t_r))

# Add pos/neg trend
res <- res %>%
  mutate(perc = case_when(
    sp == "American_Golden-Plover" ~ "-73.31",
    sp == "Bairds_Sandpiper" ~ "-5.28",
    sp == "Black-bellied_Plover" ~ "-57.39",
    sp == "Buff-breasted_Sandpiper" ~ "-58.17",
    sp == "Cackling_Goose" ~ "833.13",
    sp == "Glaucous_Gull" ~ "0",
    sp == "King_Eider" ~ "-83.34",
    sp == "Long-tailed_Duck" ~ "-65.04",
    sp == "Long-tailed_Jaeger" ~ "0",
    sp == "Pacific_Loon" ~ "0",
    sp == "Parasitic_Jaeger" ~ "0",
    sp == "Pectoral_Sandpiper" ~ "-64.40",
    sp == "Red_Knot" ~ "-94.05",
    sp == "Red-throated_Loon" ~ "21.18",
    sp == "Ruddy_Turnstone" ~ "-76.29",
    sp == "Snow_Goose" ~ "477.0",
    sp == "Tundra_Swan" ~ "3.99",
    sp == "White-rumped_Sandpiper" ~ "-26.57",
  ))
res$perc <- as.numeric(res$perc)

# Add actual trend numbers
res <- res %>%
  mutate(perc_binomial = case_when(
    sp == "American_Golden-Plover" ~ "0",
    sp == "Bairds_Sandpiper" ~ "0",
    sp == "Black-bellied_Plover" ~ "0",
    sp == "Buff-breasted_Sandpiper" ~ "0",
    sp == "Cackling_Goose" ~ "1",
    sp == "Glaucous_Gull" ~ "1",
    sp == "King_Eider" ~ "0",
    sp == "Long-tailed_Duck" ~ "0",
    sp == "Long-tailed_Jaeger" ~ "1",
    sp == "Pacific_Loon" ~ "1",
    sp == "Parasitic_Jaeger" ~ "1",
    sp == "Pectoral_Sandpiper" ~ "0",
    sp == "Red_Knot" ~ "0",
    sp == "Red-throated_Loon" ~ "0",
    sp == "Ruddy_Turnstone" ~ "0",
    sp == "Snow_Goose" ~ "1",
    sp == "Tundra_Swan" ~ "1",
    sp == "White-rumped_Sandpiper" ~ "0",
  ))
res$perc_binomial <- as.numeric(res$perc_binomial)

# Add time at range
res <- res %>%
  mutate(time = case_when(
    sp == "American_Golden-Plover" ~ "2.5",
    
    sp == "Bairds_Sandpiper" ~ "3",
    
    sp == "Black-bellied_Plover" ~ "3.5",
    sp == "Buff-breasted_Sandpiper" ~ "3.5",
    sp == "Cackling_Goose" ~ "1",
    
    sp == "Glaucous_Gull" ~ "0.5",
    
    sp == "King_Eider" ~ "4",
    sp == "Long-tailed_Duck" ~ "1.5",
    
    sp == "Long-tailed_Jaeger" ~ "1",
    sp == "Pacific_Loon" ~ "1",
    sp == "Parasitic_Jaeger" ~ "4",
    
    sp == "Pectoral_Sandpiper" ~ "2.5",
    sp == "Red_Knot" ~ "4",
    sp == "Red-throated_Loon" ~ "2.17",
    sp == "Ruddy_Turnstone" ~ "3",
    sp == "Snow_Goose" ~ "1",
    sp == "Tundra_Swan" ~ "2",
    sp == "White-rumped_Sandpiper" ~ "3.5",
  ))
res$time <- as.numeric(res$time)

# Rescaling range size
res$range_scale <- res$range_size / 1e12

write.csv(res, "res_terra_edit_moresp.csv")




