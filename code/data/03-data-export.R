# Export
out <- "data/data-grid/"
if (!file.exists(out)) dir.create(out, recursive = TRUE)
sf::st_write(grd_poly, dsn = glue("{out}grid_poly.geojson"), quiet = TRUE)
stars::write_stars(grd_ras, dsn = glue("{out}grid_raster.tif"), quiet = TRUE)