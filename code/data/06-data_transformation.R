library(stars)
filespaths <- dir(
  "data/data-format/bird-warp", 
  recursive = TRUE, 
  full.names = TRUE
)

# Create folders to export to 
output <- "data/data-format/bird-transformed/"
folders <- dir("data/data-format/bird-warp")

for(i in 1:length(folders)) dir.create(paste0(output, folders[i]), recursive = TRUE)

stressor_names <- c(
  "Built1994", "Built2009", "croplands1992", "croplands2005", "2008-inorganic", 
  "2013-inorganic", "2008-invasives", "2013-invasives","2008-night_lights", 
  "2008-ocean_pollution", "2008-plumes_fert", "2008-plumes_pest","2008-population", 
  "2008-shipping", "2013-night_lights", "2013-ocean_pollution", "2013-plumes_fert",
  "2013-plumes_pest", "2013-population", "2013-shipping", "HFP1993_int", "HFP1993.tif", 
  "HFP2009_int", "HFP2009.tif", "Lights1994", "Lights2009", "NavWater1994", "NavWater2009",
  "Pasture1993", "Pasture2009", "Popdensity1990", "Popdensity2010", "Railways", "Roads"
)

dat <- list()
for(i in 1:length(stressor_names)) {
  # Get filepaths for stressor i
  uid <- stringr::str_detect(filespaths, stressor_names[i]) 
  files <- filespaths[uid] 
  
  # Load tif files for stressor i
  stress <- lapply(files, stars::read_stars) 
  
  # Log transform all data 
  for(j in 1:length(stress)) stress[[j]] <- log(stress[[j]] + 1)
  
  # Evaluate 99th percentile for stressor i across all birds
  max_vals <- numeric(length(stress))
  for (j in 1:length(stress)) {
    max_vals[j] <- max(stress[[j]][[1]], na.rm = TRUE)
  }
  maxVal <- max(max_vals)
  
  # Standardize individual bird rasters based on 99th percentile of stressor i across all birds
  for(j in 1:length(stress)) stress[[j]] <- stress[[j]] / maxVal
  
  # Export 
  newfiles <- gsub("bird-warp","bird-transformed",files)
  for(j in 1:length(stress)) {
    stars::write_stars(
      stress[[j]],
      newfiles[j]
    )
  }
}



