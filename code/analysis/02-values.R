# In pseudo code: 

# Import halpern & venter data 
# Normalize data 
# Export data

source("./code/functions/normalization.R")
source("./code/functions/check.R")

ras <- "./data/data-format/bird-grid/"
crp <- "./data/data-format/bird-crop/"
wrp <- "./data/data-format/bird-warp/"
files <- dir(ras, pattern = ".tif$", full.names = TRUE)
bgrid <- lapply(files, stars::read_stars)

# Get bird names
spnm <- lapply(bgrid, names)
spnm <- gsub("bird-grid_", "", spnm) 
spnm <- gsub(".tif", "", spnm)

# load warps into environment
for(i in 1:length(bgrid)) {
  wrp <- paste0("./data/data-format/bird-warp/",spnm[i])
  for(j in 1:length(stress)){
  stress <- dir(wrp, pattern = ".tif$", full.names = TRUE)
 # sum = exactextractr::exact_extract(j)
  }
}



