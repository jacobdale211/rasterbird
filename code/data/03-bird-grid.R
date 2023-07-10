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
"Common-ringed Plover"

# Temp removal, no trend data


# Select only birds of interest 
brange <- brange[brange$sp %in% sp, ]

# Vector of species names without names for file export
nm <- gsub(" ","_", brange$species)

for(i in 1:nrow(brange)) {
  print(i)
  sf::st_buffer(brange[i,], 2) |> # in degrees
  stars::st_rasterize(dy = .1, dx = .1) |> # use cell-size  
  stars::write_stars(paste0(path,"bird-grid_",nm[i],".tif"))
}

# # Outside of workflow - to be changed in the future!
# 
# # Modify red knot range
test <- stars::read_stars("data/data-format/bird-grid/bird-grid_Red_Knot.tif")
# Create bounding box
bbox <- c(
  xmin = -120, ymin = -60,
  xmax = -30, ymax = 60
) |>
  sf::st_bbox(crs = sf::st_crs(4326)) |>
  sf::st_as_sfc()
crop <- brange[18,][bbox, ]

sf::st_buffer(crop, 2) |> # in degrees
  stars::st_rasterize(dy = .1, dx = .1) |> # use cell-size  
  stars::write_stars(paste0(path,"bird-grid_Red_Knot.tif"))

# Proper Red Knot range
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
