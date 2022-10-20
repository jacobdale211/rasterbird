brange = sf::st_read("./data/data-raw/birdlife/birds_multistress/Bylot_non_breeding_range.shp")
brange_small = brange[1,1]

#grid
brange_buffer <- sf::st_buffer(brange_small, 10000)
grid <- sf::st_make_grid(brange_buffer, n = c(20,20))
uid <- sf::st_intersects(brange_buffer, grid) |> unlist() |> sort()
uid
grid <- grid[uid]

grid <- stars::st_as_stars(grid)
stars::write_stars(grid, "./data/data-format/bird-grid.tif/")
