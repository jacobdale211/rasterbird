source("./code/functions/check.R")
on.exit(sf::sf_use_s2(TRUE), add = TRUE)
sf::sf_use_s2(FALSE)

brange = sf::st_read("./data/data-raw/birdlife/birds_multistress/Bylot_non_breeding_range.shp")
path <- "./data/data-format/bird-grid/"
chk_create(path)

# Species of interest
sp <- c(
  "American Golden-Plover",
  "Bairds Sandpiper",
  "Black-bellied Plover",
  "Buff-breasted Sandpiper",
  "Cackling Goose",
  "Glaucous Gull",
  "King Eider",
  "Long-tailed Duck",
  "Long-tailed Jaeger",
  "Pacific Loon",
  "Parasitic Jaeger",
  "Pectoral Sandpiper",
  "Red Knot",
  "Red-throated Loon",
  "Ruddy Turnstone",
  "Snow Goose",
  "Tundra Swan",
  "White-rumped Sandpiper"
)

# Removed
"Common-ringed Plover" # no data in study area
# Temp removal, no trend data
"Bairds Sandpiper"
"Glaucous Gull"
"Pacific Loon"


# Select only birds of interest 
brange <- brange[brange$sp %in% sp, ]

# Vector of species names without names for file export
nm <- gsub(" ","_", brange$species)

# Geojson for cropping
america_poly <- st_read("america.geojson")

for(i in 1:nrow(brange)) {
  print(i)
  buf <- sf::st_buffer(brange[i,], 1) # in degrees
  crp <- sf::st_crop(buf, america_poly)
  rast <- stars::st_rasterize(crp, dy = .1, dx = .1)  # use cell-size  
  stars::write_stars(rast, paste0(path,"bird-grid_",nm[i],".tif"))
}

# # # Outside of workflow - to be changed in the future!
# # 
# # # Modify red knot range
# test <- stars::read_stars("data/data-format/bird-grid/bird-grid_Red_Knot.tif")
# # Create bounding box
# bbox <- c(
#   xmin = -120, ymin = -60,
#   xmax = -30, ymax = 60
# ) |>
#   sf::st_bbox(crs = sf::st_crs(4326)) |>
#   sf::st_as_sfc()
# crop <- brange[18,][bbox, ]
# 
# sf::st_buffer(crop, 1) |> # in degrees
#   stars::st_rasterize(dy = .1, dx = .1) |> # use cell-size  
#   stars::write_stars(paste0(path,"bird-grid_Red_Knot.tif"))
# 
# 
# 
# # Still have not thought of a proper way to fit this into the workflow...
# # Proper Red Knot range
# brange <- sf::st_read("data/data-raw/birdlife/birds_multistress/Bylot_non_breeding_range.shp")
# redknot <- brange[18,1]
# 
# #xmin - Minimum longitude
# #xmax - Maximum longitude
# #ymin - Minimum latitude
# #ymax - Maximum latitude
# 
# bbox <- sf::st_bbox(c(xmin = -75.0, ymin = 40.5, xmax = -73.5, ymax = 42.0), crs = sf::st_crs(4326))
# cropped_multipolygon <- sf::st_intersection(redknot, bbox)
# 
# 
