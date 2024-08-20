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
for(i in 1:length(bgrid)) {
  # This is messy 
  wrpsp <- paste0(wrp,spnm[i],"/")
  crpsp <- paste0(crp,spnm[i],"/")
  chk_create(wrpsp)
  stress <- dir(crpsp, pattern = ".tif$", full.names = TRUE) |>
            lapply(stars::read_stars)
  nmst <- unlist(lapply(stress, function(x) tools::file_path_sans_ext(names(x))))
  names(stress) <- nmst
            
            
  # Iterate over stressors
  for(j in 1:16) {
    print(paste0("bird: ", i,"; stressor: ", j))  
    stars::st_warp(stress[[j]], bgrid[[i]]) |>
    stars::write_stars(
      paste0(
        wrpsp, 
        gsub("bird-crop_","bird-warp_",nmst[j]), 
        ".tif"
      )
    )
  }
}
