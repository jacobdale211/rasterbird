# This grid will not work, as there is no code that actually creates your grid. 
# If you run this it should give an error saying something like object grd_poly does not exist.
# Export
out <- "data/data-grid/"
if (!file.exists(out)) dir.create(out, recursive = TRUE)
sf::st_write(grd_poly, dsn = glue("{out}grid_poly.geojson"), quiet = TRUE)
stars::write_stars(grd_ras, dsn = glue("{out}grid_raster.tif"), quiet = TRUE)

# Also, exporting the data should be part of a script, not an individual script. What I mean is
# that, for example, you should export your cropped data at the end of the 02-data-crop.R script. 
# To me, this script here should create you study grid. So the beginning of the script should 
# create the grd_poly and grd_ras objects for you to export, and the script name should be 
# something like 03-make_grid.R