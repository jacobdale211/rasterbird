drivers <- dir(
  here::here("data/data-stressors/"),
  pattern = ".tif$",
  full.names = TRUE
)

# Had to switch venter dir from "data/data-raw/terrestrial_human_footprint_venter-103a233e",
# it was different on my computer


files <- c(drivers)
nm <- basename(files) |> tools::file_path_sans_ext()

# Select stressors to consider
uid <- c(
  "halpern_cea-4f84f0e3-2013-inorganic",
  "halpern_cea-4f84f0e3-2013-invasives",
  "halpern_cea-4f84f0e3-2013-night_lights",
  "halpern_cea-4f84f0e3-2013-plumes_fert",
  "halpern_cea-4f84f0e3-2013-plumes_pest",
  "halpern_cea-4f84f0e3-2013-population",
  "terrestrial_human_footprint_venter-103a233e-Built2009",
  "terrestrial_human_footprint_venter-103a233e-croplands2005",
  "terrestrial_human_footprint_venter-103a233e-Lights2009",
  "terrestrial_human_footprint_venter-103a233e-NavWater2009",
  "terrestrial_human_footprint_venter-103a233e-Pasture2009",
  "terrestrial_human_footprint_venter-103a233e-Popdensity2010",
  "terrestrial_human_footprint_venter-103a233e-Railways",
  "terrestrial_human_footprint_venter-103a233e-Roads"
)
#   "halpern_cea-4f84f0e3-2013-shipping"  "halpern_cea-4f84f0e3-2013-ocean_pollution"  removed for visualization purposes
uid <- nm %in% uid
files <- files[uid]
nm <- nm[uid]

# Bounding box around the Americas
aoi <- c(
  xmin = -169, ymin = -60,
  xmax = -30, ymax = 90
) |>
sf::st_bbox(crs = sf::st_crs(4326)) |>
sf::st_as_sfc() |>
sf::st_as_sf()

# Grid
grd <- stars::st_rasterize(aoi, dy = .1, dx = .1) #|> # use cell-size
# stars::write_stars("data/data-grid/grid.tif"))

# All subsequent steps are wrapped in a loop to decrease memory usage
# NOTE: Still running into some memory issues for some stressors
# To address this:
# usethis::edit_r_environ()
# add when the tab opens up in R studio, add this to the 1st line: R_MAX_VSIZE=100Gb
for(i in 1:length(files)) {
  print(paste0(i," of ", length(files)))

  # Load data
  temp <- stars::read_stars(files[i], proxy = TRUE)

  # Warp data
  dat <- stars::st_warp(temp, grd)

  # Mask data
  dat <- dat[aoi]

  # Log transformation
   dat <- log(dat + 1)

  # Standardize
  md <- max(dat[[1]], na.rm = TRUE)
  dat <- dat/md

  # Export
  output <- here::here("data","data-stressors")
  if(!file.exists(output)) dir.create(output, recursive = TRUE)
  stars::write_stars(
    dat,
    here::here(output, glue::glue("{nm[i]}.tif")),
    delete_dsn = TRUE
  )
}


# Cumulative data
stressors <- dir(
  output,
  pattern = ".tif$",
  full.names = TRUE
) |>
lapply(stars::read_stars, proxy = TRUE)

library(stars)
cumul <- do.call("c", stressors) |>
         stars::st_redimension() |>
         stars::st_apply(c(1,2), sum, na.rm = TRUE)

output2 <- "data/data-cumulative_stressors/"
if(!file.exists(output2)) dir.create(output2, recursive = TRUE)         
# hashed out to rerun image formaatting, make sure to readd when rerunning full script
 stars::write_stars(cumul, here::here(output2, "cumulative_stressors.tif"))
dat <- stars::read_stars(here::here(output2, "cumulative_stressors.tif"))
 dat[[1]][dat[[1]] <= 0] <- NA
brange = sf::st_read("./data/data-raw/birdlife/birds_multistress/Bylot_non_breeding_range.shp")
a_f <- sf::st_read("a_f.gpkg")

  out <- "figures/" 
  if(!file.exists(out)) dir.create(out, recursive = TRUE)         
  png("figures/cumulative_stressors.png", res = 400, width = 200, height = 200, bg = viridis::viridis(900), units = "mm", pointsize = 24)
  par(mar = c(0,0,0,0))
  image(dat, col = viridis::viridis(100))
  # plot(sf::st_geometry(brange[18,]), add = TRUE, border = "#A1B866", lwd = 2)
  # plot(sf::st_geometry(brange[25,]), add = TRUE, border = "#F6C143", lwd = 2)
  dev.off()
  
  plot(cumul)

