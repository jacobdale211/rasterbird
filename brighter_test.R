drivers <- dir(
  here::here("data/data-stressors/"),
  pattern = ".tif$",
  full.names = TRUE
)
stressors <- lapply(drivers, stars::read_stars)




#log transformation
source("./code/functions/check.R")
path <- "./output/brighter"
chk_create(path)
nm <- c(
  "halpern_cea-4f84f0e3-2013-inorganic",
  "halpern_cea-4f84f0e3-2013-invasives",
  "halpern_cea-4f84f0e3-2013-night_lights",
  "halpern_cea-4f84f0e3-2013-ocean_pollution",
  "halpern_cea-4f84f0e3-2013-plumes_fert",
  "halpern_cea-4f84f0e3-2013-plumes_pest",
  "halpern_cea-4f84f0e3-2013-population",
  "halpern_cea-4f84f0e3-2013-shipping"
)
for (i in 1:8) {
  upd <- log(stressors[[i]] + 1)
  stars::write_stars(upd, paste0(path,nm[i],".tif"))
}


image(stressors[[1]])
x <- stressors[[1]]
x[x == 0] <- NA
image(x)
y = log(x+1)
y2 = log(y+1)
y3 = log(y2+1)
y4 = log(y3+1)


par(bg = "#550c54")
image(x, col = viridis::viridis(900))
image(y, col = viridis::viridis(900))
image(y2, col = viridis::viridis(900))
image(y3, col = viridis::viridis(900))
image(y4, col = viridis::viridis(900))


z <- stressors[[8]]
image(z)
z[z == 0] <- NA
image(z, col = viridis::viridis(900))



# adding americas map
library(rnaturalearth)
world <- ne_countries(scale = "medium", returnclass = "sf")
americas <- subset(world, continent %in% c("North America", "South America"))

america_poly <- sf::st_read("map.geojson")
world <- ne_countries(scale = "medium", returnclass = "sf")
std_ar <- subset(world, continent %in% c("North America", "South America"))
std_ar <- sf::st_crop(std_ar, america_poly)
plot(std_ar[1], col="#3B0849")
class(std_ar)

# study <- stars::st_as_stars(std_ar[1])
# library(ggplot2)
# library(stars)

# maptry <- sf::st_read("custom.geojson")
# plot(maptry[1])
# 
# study$featurecla <- as.numeric(study$featurecla)
# study[is.na(study)] <- 1

z <- stressors[[3]]  
image(z)
z[z == 0] <- NA

par(bg="#550c54")
# par(bg="white")
# ggplot() +
#   geom_stars(data = z) +
#   geom_stars(data = study) +
#   theme(panel.background = element_rect(fill = "white"))

par(bg="#550c54")
plot(st_geometry(americas), col = "#3d1151", border = "#3d1151", 
     xlim = c(-130, -30), ylim = c(-60, 90))
image(z, add = TRUE, col = viridis::viridis(100))



# test <- sf::st_as_sf(x, as_points=FALSE, merge=TRUE)
# buf <- sf::st_buffer(test, dist = 0.0001)
# buf_star <- stars::st_as_stars(buf)
# plot(buf)
# image(buf_star)
# 
# plot(buf, col = viridis::mako(900), main = NULL)
# 
# 

# world <- ne_countries(scale = "medium", returnclass = "sf")
# 
# # Filter to include only North and South America
# americas <- subset(world, continent %in% c("North America", "South America"))
# 
# lon <- seq(-130, -30, length.out = 100)
# lat <- seq(-60, 60, length.out = 100)
# vals <- matrix(runif(10000, min = 0, max = 1), ncol = 100)
# dummy_stars <- st_as_stars(vals, dimensions = st_dimensions(x = lon, y = lat))
# 
# # Plot the map
# plot(st_geometry(americas), col = "lightgray", border = "darkgray", main = "Dummy Spatial Data on Americas Map")
# 
# # Overlay the dummy stars object
# image(dummy_stars, add = TRUE, col = terrain.colors(10))
