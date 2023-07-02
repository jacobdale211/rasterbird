on.exit(sf::sf_use_s2(TRUE), add = TRUE)
sf::sf_use_s2(FALSE)

cumul <- stars::read_stars("output/data-stressors/terrestrial_human_footprint_venter-103a233e-croplands2005.tif")
v <- read.csv("data/data-format/v-matrix.csv")
# cumul <- dir("output/data-stressors", full.names = TRUE) |>
  # lapply(stars::read_stars)

brange = sf::st_read("./data/data-raw/birdlife/birds_multistress/Bylot_non_breeding_range.shp")
species <- brange$species

pop <- read.csv("data/data-raw/birdlife/birdlife-trends.csv")


l <- list()
for(i in 1:nrow(brange)) {
  l[[i]] <- cumul[brange[i, ]]
}

m <- list()
for(i in 1:length(l)) {
  dat <- as.data.frame(l[[i]])
  colnames(dat)[3] <- "data"
  dat <- dplyr::filter(dat, !is.na(data))
  m[[i]] <- dat$data
}

means <- lapply(m, mean, na.rm = TRUE) |> unlist()
sds <- lapply(m, sd, na.rm = TRUE) |> unlist()

dat <- data.frame(
  bird = species, 
  exposure_mean = means,
  exposure_sd = sds
  )

# Joining exposure and Rosenberg data
alldat <- dplyr::left_join(
  dat, 
  pop[,c("species","perc","Loss_med")], 
  by = c("bird" = "species")
) |>
  na.omit()

v[,5] <- as.numeric(v[,5])
alldat$exposure_mean <- as.numeric(alldat$exposure_mean)
new <- (v[,5] * alldat$exposure_mean)

# rdat <- range(alldat, na.rm = TRUE)
par(mar = c(20,20,20,20))

birds <- alldat$bird
ggplot(data = alldat, aes(x = alldat$exposure_mean, y = alldat$perc)) +
  geom_point() +
  geom_text(aes(label = birds), vjust = -0.5) +
  labs(x = "Exposure Mean", y = "% Population Change (in 100's)", title = "Exposure mean vs. population trends of Cropland Expansion")


plot(x = alldat$exposure_mean, y = alldat$perc)
#pch = 21, col = "#88c3b5", bg = "#66281b", xlab = "", xaxt = "n")
for(i in 1:length(m)) {
 # points(x = rep(i, length(m[[uid[i]]])), y = m[[uid[i]]], cex = .1, col = "#00000033")
}
mtext(species[uid], side = 1, at = uid, las = 2, line = 1)
points(x = seq(1, length(m)), y = means[uid], pch = 21, col = "#88c3b5", bg = "#66281b", cex = 2)

means
