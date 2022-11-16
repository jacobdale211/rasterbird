source("./code/functions/check.R")
# source("./code/data/02-data-import.R")
ras <- "./data/data-format/bird-grid/"
crp <- "./data/data-format/bird-crop/"
wrp <- "./data/data-format/bird-warp/"
chk_create(wrp)
files <- dir(ras, pattern = ".tif$", full.names = TRUE)
bgrid <- lapply(files, stars::read_stars)

# Get bird names
spnm <- lapply(bgrid, names)
spnm <- gsub("bird-grid_", "", spnm) 
spnm <- gsub(".tif", "", spnm) 

# Loop to crop each stressor over each species range
for(i in 2:length(bgrid)) {
  # This is messy 
  wrpsp <- paste0(wrp,spnm[i],"/")
  crpsp <- paste0(crp,spnm[i],"/")
  chk_create(wrpsp)
  stress <- dir(crpsp, pattern = ".tif$", full.names = TRUE) |>
            lapply(stars::read_stars)
            
  # Iterate over stressors
  for(j in 1:34) {
    print(paste0("bird: ", i,"; stressor: ", j))  
    grd <- sf::st_transform(
      bgrid[[i]], 
      crs = sf::st_crs(stress[[j]])
    )
    stars::st_warp(stress[[j]], grd) |>
    sf::st_transform(crs = 4326) |>
    stars::write_stars(
      paste0(
        wrpsp,
        gsub("bird-crop_","bird-warp_",spnm[i],names(stress[[j]]))
      )
    )
  }
}
