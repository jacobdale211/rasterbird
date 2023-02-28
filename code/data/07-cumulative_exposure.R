source("./code/functions/check.R")
plt <-"figures/cumulative/"
tif <-"output/cumulative"
chk_create(plt)
chk_create(tif)

str <- "figures/stressors/"
chk_create(str)

ras <- "./data/data-format/bird-grid/"
files <- dir(ras, pattern = ".tif$", full.names = TRUE)
bgrid <- lapply(files, stars::read_stars)

# Names for plots
cumul_nam <- lapply(bgrid, names)
cumul_nam <- gsub("bird-grid_", "cumulative_", cumul_nam) 
cumul_nam <- gsub(".tif", "", cumul_nam) 

# Get bird names
spnm <- lapply(bgrid, names)
spnm <- gsub("bird-grid_", "", spnm) 
spnm <- gsub(".tif", "", spnm) 

library(stars)
birdfolders <- dir("data/data-format/bird-vulnerability", full.names = TRUE)

# Red knot is i=9, snow goose is i=11

for(i in 1:length(birdfolders)) {
  input <- cumul_nam[[i]]
  
  dat <- dir(
    birdfolders[i],
    pattern = ".tif$", 
    full.names = TRUE
  ) |>
  lapply(stars::read_stars, proxy = FALSE)
  
  # Cumulative plots of everything 
  cumul <- do.call("c", dat) |>
           st_redimension() |>
           st_apply(c(1,2), sum, na.rm = TRUE)
  
  # Export tif file
  stars::write_stars(cumul, glue::glue("output/cumulative/{input}.tif"))
  # Export
  par(mfrow = c(1,1))
  png(file = glue::glue("figures/cumulative/{input}.png"))
  image(cumul, col = viridis::viridis(100), main = "")
  dev.off()
}

# New folder - export each iteration of cumulative effect
# one tif file per species



# # Outside of workflow - to be changed in the future!
# 
# # Modify red knot range
# test <- stars::read_stars("data/data-format/bird-grid/bird-grid_Red_Knot.tif")
# # Create bounding box
# bbox <- c(
#   xmin = -120, ymin = -60, 
#   xmax = -30, ymax = 60
# ) |>
#   sf::st_bbox(crs = sf::st_crs(4326)) |>
#   sf::st_as_sfc()
# 
# # Proper Red Knot range
# cumul_crop <- cumul[bbox, ]
# plot(cumul_crop)
# 
# polygon_1 <- snowgoose[bbox, ]
# polygon_1
# # Getting average exposure
# brange <- sf::st_read("data/data-raw/birdlife/birds_multistress/Bylot_non_breeding_range.shp")
# redknot <- brange[18,1]
# cumul_new<- cumul_crop[redknot]
# 
# snowgoose <-brange[25,1]
# snowgoose <- stars::read_stars("data/data-format/bird-grid/bird-grid_Snow_Goose.tif")
# cumul_new <-cumul[snowgoose]

