path <- dir("/Users/jacobdale/tryitout/rasterbird/data/data-raw/", pattern = ".tif", full.names = TRUE)
dat <- lapply(path, read_stars, proxy = TRUE)
dat <- lapply(dat, st_transform, crs = 4326)