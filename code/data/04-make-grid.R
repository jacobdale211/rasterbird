# Load global parameters
# See code/functions/global_parameters.R 
# You need to load all functions in the code/functions/ folder, which is currently done at 
# line 4 in the ./_pipeline.R script
global_parameters() 

# Bounding box of interest or polygon to create grid 
bbox <- unlist(param$grid$bbox) # If bounding box 
# x <- sf::st_read("path/to/file") # If polygon

# Create grid 
## With bounding box 
pipedat::pipegrid(
  bbox = bbox, 
  cellsize = param$grid$cellsize, 
  crs = param$grid$crs
) 

## With polygon 
# pipedat::pipegrid(
#   x = x, 
#   cellsize = param$grid$cellsize, 
#   crs = param$grid$crs
# ) 