x_raster <- as(x2, "Raster")
exact_extract(x_raster, brange_buffer, function(values, coverage_fraction)
  sum(coverage_fraction))
avg <- exact_extract(x_raster, grid, 'mean')
