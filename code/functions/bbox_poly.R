# I would put functions that you create in a seperate folder rather than in the scripts themselves, 
# unless there is a good reason to only put it in a script
# In this particular case, it's a function that you could reuse in multiple scripts.
bbox_poly <- function(bbox, crs) { 
  sf::st_bbox(bbox, crs = sf::st_crs(crs)) |> sf::st_as_sfc() 
}