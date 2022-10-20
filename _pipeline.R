pipeline <- function() {
  # Functions 
  # Loads all functions in `code/functions/` to memory
  lapply(dir("code/functions", full.names = TRUE), source) 
  
  # Data 
  source("code/data/01-data-download.R")
  source("code/data/02-data-crop.R")
  source("code/data/03-make-grid.R")
  
  # Analyses 
  # I guess you see where I'm going with his.. :) 
  
  # Figures 
  
  # Report 
}