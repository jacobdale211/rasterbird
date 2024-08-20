on.exit(sf::sf_use_s2(TRUE), add = TRUE)
sf::sf_use_s2(FALSE)



# Means for stressors using data-stressors data




# New population trends for shorebirds
  new <- read.csv("All_Shorebird_migration_survey_wide_trends.csv")
  sbirds <- new[new$trend_type == "Long-term",]

# Get species names
  brange = sf::st_read("./data/data-raw/birdlife/birds_multistress/Bylot_non_breeding_range.shp")
  species <- brange$species

# Filter
  filter <- sbirds[sbirds$species %in% species, ]
  final <- filter[c("species", "percent_change")]

# Old trends
  pop <- read.csv("data/data-raw/birdlife/birdlife-trends.csv")
  pop <-pop[c("species", "percent_change")]

# Join
  dat <- rbind(final, pop)

# cumul <- stars::read_stars("output/data-stressors/terrestrial_human_footprint_venter-103a233e-croplands2005.tif")

filespaths <- dir("data/data-format/bird-grid", recursive = TRUE, full.names = TRUE)

c <- stringr::str_detect(filespaths, "American") 

cr <- filespaths[c]
crops <- lapply(cr, stars::read_stars) 

means <- lapply(crops, mean, na.rm = TRUE) |> unlist()

image(crops[[1]])

