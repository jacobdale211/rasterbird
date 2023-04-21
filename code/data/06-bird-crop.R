source("./code/functions/check.R")
source("./code/data/02-data-import.R")
ras <- "./data/data-format/bird-grid/"
crp <- "./data/data-format/bird-crop/"
chk_create(crp)
files <- dir(ras, pattern = ".tif$", full.names = TRUE)
bgrid <- lapply(files, stars::read_stars)

# Get bird names
spnm <- lapply(bgrid, names)
spnm <- gsub("bird-grid_", "", spnm) 
spnm <- gsub(".tif", "", spnm) 

# Loop to crop each stressor over each species range
for(i in 14:length(bgrid)) {
  crpsp <- paste0(crp,spnm[i],"/")
  chk_create(crpsp)
  for(j in 17:length(stress)) {
    print(paste0("bird: ", i,"; stressor: ", j))  
    grd <- sf::st_transform(
      bgrid[[i]], 
      crs = sf::st_crs(stress[[j]])
    )
    sf::st_crop(stress[[j]], grd) |>
    stars::write_stars(paste0(crpsp,"bird-crop_",spnm[j],"-",names(stress[[j]])))
  }
}

