path <- dir("./data/data-raw/", pattern = ".tif", full.names = TRUE)
dat <- lapply(path, read_stars, proxy = TRUE)


venter <- stars::read_stars("./data/data-raw/venter/Dryadv3/Maps/croplands2005.tif")
summary(venter)
