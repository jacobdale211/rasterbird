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
  "Common-ringed Plover",
  "Glaucous Gull",
  "King Eider",
  "Long-tailed Duck",
  "Pacific Loon",
  "Pectoral Sandpiper",
  "Red Knot",
  "Red-throated Loon",
  "Ruddy Turnstone",
  "Snow Goose",
  "Tundra Swan",
  "White-rumped Sandpiper"
)

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


