on.exit(sf::sf_use_s2(TRUE), add = TRUE)
sf::sf_use_s2(FALSE)

cumul <- dir("output/cumulative", full.names = TRUE) |>
  lapply(stars::read_stars)

brange = sf::st_read("./data/data-raw/birdlife/birds_multistress/Bylot_non_breeding_range.shp")
species <- brange$species
pop <- read.csv("data/data-raw/birdlife/birdlife-trends.csv")

m <- list()
for(i in 1:length(cumul)) {
  dat <- as.data.frame(cumul[[i]])
  colnames(dat)[3] <- "data"
  dat <- dplyr::filter(dat, !is.na(data))
  m[[i]] <- dat$data
}

means <- lapply(m, mean, na.rm = TRUE) |> unlist()
sds <- lapply(m, sd, na.rm = TRUE) |> unlist()

nm <- lapply(cumul, names) 
nm <- gsub("cumulative_","",nm)
nm <- gsub(".tif","",nm)
nm <- gsub("_", " ",nm)

dat <- data.frame(
  bird = nm, 
  exposure_mean = means,
  exposure_sd = sds
)

# Joining exposure and Rosenberg data
dat <- dplyr::left_join(
  dat, 
  pop[,c("species","perc","Loss_med")], 
  by = c("bird" = "species")
) |>
  na.omit()


par(mar = c(12,2,2,2))
plot(x = dat$exposure_mean, y = dat$perc)

plot(x = dat$exposure_mean[-c(10,4)], y = dat$perc[-c(10,4)])

# pch = 21, col = "#88c3b5", bg = "#66281b", xlab = "", xaxt = "n")
for(i in 1:length(m)) {
  # points(x = rep(i, length(m[[uid[i]]])), y = m[[uid[i]]], cex = .1, col = "#00000033")
}
mtext(species[uid], side = 1, at = uid, las = 2, line = 1)
points(x = seq(1, length(m)), y = means[uid], pch = 21, col = "#88c3b5", bg = "#66281b", cex = 2)

means