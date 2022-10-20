source("./code/figures/01-studyarea.R")
source("./code/figures/02-stressors.R")

x_raster <- as(x3, "Raster")
exact_extract(x_raster, brange_buffer, function(values, coverage_fraction)
  sum(coverage_fraction))
avg <- exactextractr::exact_extract(x_raster, grid, 'mean')

grid<- sf::st_sf(grid)
grid$avg = avg
plot(grid, main = "")
