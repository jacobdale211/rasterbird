# Multipolygon to polygon

library(stars)
library(sf)

data_dir <- "data/data-format/bird-grid"
files <- list.files(data_dir, pattern = ".tif", full.names = TRUE)

p <- list()
for (file in files) {
  obj <- stars::read_stars(file)
  polygon <- sf::st_as_sf(obj)
  p[[file]] <- polygon
}

# Remove 2 (Bairds, no pop trends)
p <- p[-2]
# Remove 9 (Pacific Loon, not in study area)
p <- p[-9]



# Birds that need to have separated ranges:
# 2, 4, 6, 7, 8, 9, 10, 11*, 13, 14, 16

# # # Individualizing bird ranges # # #

# Note: buffer of 2 degrees applied already in bird grid.. no need to reapply
# for restructuring of ranges 

# 1, American Golden-Plover, 1 range
amgplo_u <- sf::st_union(p[[1]])
amgplo_d <- sf::st_cast(amgplo_u, "POLYGON")

# 2, Black-bellied_Plover
blbplo_u <- sf::st_union(p[[2]])
blbplo_d <- sf::st_cast(blbplo_u, "POLYGON")

# 3, Buff-breasted Sandpiper, 1 range
bubsan_u <- sf::st_union(p[[3]])
bubsan_d <- sf::st_cast(bubsan_u, "POLYGON")

# 4, Cackling Goose
cacgoo_u <- sf::st_union(p[[4]])
cacgoo_d <- sf::st_cast(cacgoo_u, "POLYGON")


# 5, Glaucous Gull, 1 range
glagul_u <- sf::st_union(p[[6]])
glagul_d <- sf::st_cast(glagul_u, "POLYGON")

# 6, King Eider
kineid_u <- sf::st_union(p[[6]])
kineid_d <- sf::st_cast(kineid_u, "POLYGON")

# 7, Long-tailed Duck
lotduc_u <- sf::st_union(p[[7]])
lotduc_d <- sf::st_cast(lotduc_u, "POLYGON")

# 8, Long-tailed Jaeger
lotjae_u <- sf::st_union(p[[8]])
lotjae_d <- sf::st_cast(lotjae_u, "POLYGON")

# 9, Parasitic Jaeger
parjae_u <- sf::st_union(p[[9]])
parjae_d <- sf::st_cast(parjae_u, "POLYGON")

# 10, Pectoral Sandpiper
pecsan_u <- sf::st_union(p[[10]])
pecsan_d <- sf::st_cast(pecsan_u, "POLYGON")

# 11, Red Knot * need to remove EU and Africa populations
redkno_u <- sf::st_union(p[[11]])
redkno_d <- sf::st_cast(redkno_u, "POLYGON")

# 12, Red-throated Loon, 1 range
retloo_u <- sf::st_union(p[[12]])
retloo_d <- sf::st_cast(retloo_u, "POLYGON")

# 13, Ruddy Turnstone * need to remove EU and Africa populations
rudtur_u <- sf::st_union(p[[13]])
rudtur_d <- sf::st_cast(rudtur_u, "POLYGON")

# 14, Snow Goose
snogoo_u <- sf::st_union(p[[14]])
snogoo_d <- sf::st_cast(snogoo_u, "POLYGON")

# 15, Tundra Swan, 1 range
tunswa_u <- sf::st_union(p[[15]])
tunswa_d <- sf::st_cast(tunswa_u, "POLYGON")

# 16, White-rumped Sandpiper
whrsan_u <- sf::st_union(p[[16]])
whrsan_d <- sf::st_cast(whrsan_u, "POLYGON")

# Removing EU and Africa polygons
redkno_d <- redkno_d[-3] # Run 3 times
rudtur_d <- rudtur_d[-2] # Run 5 times

# Extract values from rasters
drivers <- dir("data/data-stressors", full.names = TRUE) |>
  lapply(stars::read_stars)

list_birds <- c(amgplo_d, blbplo_d, bubsan_d, cacgoo_d, glagul_d, kineid_d, lotduc_d, lotjae_d, parjae_d,
                pecsan_d, redkno_d, retloo_d, rudtur_d, snogoo_d, tunswa_d, whrsan_d)

list_num_birds <- c(1, 4, 1, 2, 3, 3, 9, 13, 6, 2, 2, 1, 1, 2, 1, 3)
list_name_birds <- c("American Golden-Plover", "Black-bellied_Plover", "Buff-breasted Sandpiper", "Cackling Goose", "Glaucous Gull",
                     "King Eider", "Long-tailed Duck", "Long-tailed Jaeger", "Parasitic Jaeger", "Pectoral Sandpiper", "Red Knot",
                     "Red-throated Loon", "Ruddy Turnstone", "Snow Goose", "Tundra Swan", "White-rumped Sandpiper")
list_name_drivers <- c(
  "inorganic",
  "invasives",
  "lights",
  "ocean_pollution",
  "plumes_fert",
  "plumes_pest",
  "population",
  "shipping",
  "Built2009",
  "croplands2005",
  "Lights2009",
  "NavWater2009",
  "Pasture2009",
  "Popdensity2010",
  "Railways",
  "Roads")

final_df <- data.frame(species = rep(NA, 864),
                       mean = rep(NA, 864),
                       max = rep(NA, 864),
                       min = rep(NA, 864),
                       sd = rep(NA, 864))
iterator <- 1
i=1
j=0
k=1
# loop once per bird type
for (i in 1:length(list_num_birds)) {
  current_bird <- list_birds[[i]]
  current_num <- list_num_birds[[i]]
  
  # loop for each range per bird
  for (j in 0:(current_num - 1)) {
    bird_range <- sf::st_sfc(sf::st_polygon(list_birds[[i + j]]), crs = 4326)
    # loop for each driver for each range
    for (k in 1:length(drivers)) {
      dat <- sf::st_as_sf(drivers[[k]])
      intersection <- sf::st_intersects(bird_range, dat)

      uid <- unique(unlist(intersection))
      
      # Subset the data based on unique intersected rows
      a <- dat[uid,]
      
      # Calculate mean, max, and min for the first column
      mean <- mean(a[[1]])
      max <- max(a[[1]])
      min <- min(a[[1]])
      sd <- sd(a[[1]])
      
      # Add the calculated values to the data frame within the loop
      final_df[iterator, "species"] <- paste(list_name_birds[[i]],j+1,list_name_drivers[[k]], sep = " ")
      final_df[iterator, "mean"] <- mean
      final_df[iterator, "max"] <- max
      final_df[iterator, "min"] <- min
      final_df[iterator, "sd"]  <- sd
      iterator = iterator + 1
      print(iterator)
    }
  }
  print(list_name_birds[[i]])
  i <- i + current_num
}

# Export dataframe
write.csv(final_df, file = "final_df.csv", row.names = FALSE)

# Read in csv
reformatted_data <- read.csv("final_df.csv")

# Check distributions
hist(reformatted_data$mean) 
hist(reformatted_data$max)
hist(reformatted_data$min)
hist(reformatted_data$sd)

# Need to remove NA's
final_df_filtered <- na.omit(reformatted_data)

# Recheck distributions (the same)
hist(final_df_filtered$mean) 
hist(final_df_filtered$max)
hist(final_df_filtered$min)
hist(final_df_filtered$sd)

# Population trend distributions (not accurate because of the way data is formatted in this DF)
hist(final_df_filtered$percent)
# Still lots of negatives!



# Let's try a model

model <- glm(percent ~ mean * category, data = final_df_filtered, family = "poisson")
summary(model)

# Binomial and quasibinomial ... "Error in eval(family$initialize) : y values must be 0 <= y <= 1"
# which makes sense... so should we use gaussian instead?

# "Binomial distribution only counts two states, typically represented as 
# 1 (for a success) or 0 (for a failure), given a number of trials in the data."

# Lots of 0 values.. perhaps negative binomial or Poisson (cant do Poisson because of negative
# population trends) instead of binomial?




