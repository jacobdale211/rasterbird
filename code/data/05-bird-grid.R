source("./code/functions/check.R")

brange = sf::st_read("./data/data-raw/birdlife/birds_multistress/Bylot_non_breeding_range.shp")
path <- "./data/data-format/bird-grid/"
chk_create(path)
sp <- brange$species

for(i in 1:nrow(brange)) {
  brange_buffer <- sf::st_buffer(brange[i,], 10000)
  on.exit(sf::sf_use_s2(TRUE), add = TRUE)
  sf::sf_use_s2(FALSE)
  grid <- sf::st_make_grid(brange_buffer, n = c(20,20))
  uid <- sf::st_intersects(brange_buffer, grid) |> unlist() |> sort()
  grid <- grid[uid]
  grid <- stars::st_as_stars(grid)  
  stars::write_stars(
    grid, 
    paste0("./data/data-format/bird-grid/bird-grid_",sp[i],".tif")
  )
}

