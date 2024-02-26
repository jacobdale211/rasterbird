library(terra)
library(sf)
library(exactextractr)

sf_use_s2(FALSE)

america_poly <- st_read("data/data-raw/america.geojson")

shp <- st_read(
  "data/data-raw/birdlife/birds_multistress/Bylot_non_breeding_range.shp")

# Specify desired bird ranges here 
indices <- c(1, 3, 5, 6, 10, 12, 16, 18, 20, 22, 25, 27, 28)
# Get bird ranges 
shp <- shp[indices,]

shp <- st_crop(shp, america_poly)

# 1 deg buffer
shp_buf <- st_buffer(shp, dist = 1)

extract_bird_data <- function(raster_file, shp) {
  r <- rast(raster_file)
  plot(shp)
  plot(shp_buf)

  res <- exact_extract(r, shp, include_cols = "species", include_xy = TRUE)

  res <- do.call(rbind, res)
}


# List of all drivers 
files <- list.files("data/data-stressors", full.names = TRUE)

res <- lapply(files, extract_bird_data, shp = shp_buf)

write.csv(res, "res_terra.csv")


