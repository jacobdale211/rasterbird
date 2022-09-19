bbox_poly <- function(bbox, crs) { sf::st_bbox(bbox, crs = sf::st_crs(crs)) |> sf::st_as_sfc() }
bbox <- c(xmin = -67, ymin = 39, xmax = -54, ymax = 47)
bbox_crs = 4326
data_crs <- st_crs(dat[[1]])
bb <- bbox_poly(bbox, bbox_crs) 
bb <- sf::st_transform(bb, crs = data_crs)
bb <- sf::st_crop(dat[[1]], bb)
dat_new <- lapply(dat, st_crop, bb)