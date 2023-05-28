# Load in eBird data

# library(ebirdst)
# library(raster)
# library(sf)
# library(exactextractr)
# library(dplyr)
# library(tidyr)
# library(rnaturalearth)
# library(ggplot2)
# extract <- raster::extract

# Look at bird list
x <- ebirdst_runs

# Modify data to take birds with modeled summering ranges
list <- subset(x, breeding_range_modeled == "TRUE")
write.csv(list, file = "new_ebird_list.csv")

# Download data
path <- ebirdst_download("tunswa")

# If the data package has already been downloaded, use:
# path <- get_species_path("amgplo")

# Load seasonal mean relative abundance at low resolution
abd_seasonal <- load_raster(path, 
                            product = "abundance", 
                            period = "seasonal",
                            metric = "mean",
                            resolution = "lr") 

# Get the seasons corresponding to each layer
names(abd_seasonal)

ebirdst_runs %>% 
  filter(common_name == "Tundra Swan") %>% 
  glimpse()

# NOTE files are global and will need to be cropped to Americas range

# Plot function ( to be moved )
# View("/data/data-raw/ebirdst/2020/kineid/seasonal/kineid_abundance_full-year_max_hr_2020.tif")
# plot(ex)
# 
# exposure <- function(dat) {
#   exp <- function(dat, all_stress)
# }
# 
# bird <- file(x)
# ev <- exposure(bird)

source("./code/functions/check.R")
source("./code/data/02-data-import.R")
y <- stars::read_stars("data/data-raw/ebirdst/2020/kineid/seasonal/kineid_abundance_full-year_max_hr_2020.tif")
#image(y)

x <- (stress[[1]])

raster::crs(x) <- "EPSG:4326"

grd <- sf::st_transform(y, crs = sf::st_crs(x))
sf::st_crop(x, grd) |>
stars::write_stars(paste0("bird-crop_test-",names(x)))

kin <- stars::read_stars("bird-crop_test-halpern_cea-4f84f0e3-2008-inorganic.tif")
plot(kin)                                                    
kin
