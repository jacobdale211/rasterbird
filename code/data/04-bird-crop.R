source("./code/functions/check.R")

# Temp disabled using new source
# source("./code/data/02-data-import.R")
ras <- "./data/data-format/bird-grid/"
crp <- "./data/data-format/bird-crop/"
chk_create(crp)
files <- dir(ras, pattern = ".tif$", full.names = TRUE)
bgrid <- lapply(files, stars::read_stars)

stressdir <- "./data/data-stressors/"
files2 <- dir(stressdir, pattern = ".tif$", full.names = TRUE)
stress <- lapply(files2, stars::read_stars)

# Get bird names
spnm <- lapply(bgrid, names)
spnm <- gsub("bird-grid_", "", spnm) 
spnm <- gsub(".tif", "", spnm) 

# Loop to crop each stressor over each species range
for(i in 1:length(bgrid)) {
  crpsp <- paste0(crp,spnm[i],"/")
  chk_create(crpsp)
  for(j in 1:length(stress)) {
    print(paste0("bird: ", i,"; stressor: ", j))  
    grd <- sf::st_transform(
      bgrid[[i]], 
      crs = sf::st_crs(stress[[j]])
    )
    sf::st_crop(stress[[j]], grd) |>
    stars::write_stars(paste0(crpsp,"bird-crop_",spnm[i],"-",names(stress[[j]])))
  }
}
#x <- raster::raster("/Users/jacobdale/tryitout/rasterbird/data/data-format/bird-warp/American_Golden-Plover/bird-warp_American_Golden-Plover-halpern_cea-4f84f0e3-2008-invasives.tif")
