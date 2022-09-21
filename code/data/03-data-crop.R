# Import data 
path <- dir("./data/data-raw/halpern/", pattern = ".tif", full.names = TRUE)
halpern <- lapply(path, stars::read_stars, proxy = TRUE)

# I would assume that there will be more than one raster for venter as well
path <- dir("./data/data-raw/venter/Dryadv3/Maps/", pattern = ".tif", full.names = TRUE)
venter <- lapply(path, stars::read_stars, proxy = TRUE)

# Crop data
## Bounding box
bbox <- c(xmin = -67, ymin = 39, xmax = -54, ymax = 47)
bbox_crs = 4326
bb <- bbox_poly(bbox, bbox_crs) 

# Crop halpern
data_crs <- st_crs(halpern[[1]])
bb <- sf::st_transform(bb, crs = data_crs)
halpern <- lapply(halpern, sf::st_crop, bb)
# Once this is done, you should export it to disk in `data/data-format/` to use for your analyses
nm <- names(halpern) # Might not work, you may have to play around with this.
for(i in 1:length(halpern)) {
  sf::st_write(
    halpern[[i]], 
    dsn = glue::glue("data/data-format/halpern-{nm}.tif")
  )
} 

# Crop venter
data_crs <- st_crs(venter[[1]])
bb <- sf::st_transform(bb, crs = data_crs)
venter <- lapply(venter, sf::st_crop, bb)
# Once this is done, you should export it to disk in `data/data-format/` to use for your analyses
nm <- names(venter) # Might not work, you may have to play around with this.
for(i in 1:length(venter)) {
  sf::st_write(
    venter[[i]], 
    dsn = glue::glue("data/data-format/venter-{nm}.tif")
  )
} 

# Small brange
brange_small = brange[1:1,]
sf::st_area(brange_small)
