on.exit(sf::sf_use_s2(TRUE), add = TRUE)
sf::sf_use_s2(FALSE)
cumul <- stars::read_stars("data/data-cumulative_stressors/cumulative_stressors.tif")
brange = sf::st_read("./data/data-raw/birdlife/birds_multistress/Bylot_non_breeding_range.shp")
species <- brange$species

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
uid <- order(means)
sds <- lapply(m, sd, na.rm = TRUE) |> unlist()
alldat <- unlist(m)
rdat <- range(alldat, na.rm = TRUE)
par(mar = c(12,2,2,2))
plot(means[uid], xlim = c(0, length(m)), ylim = c(0, rdat[2]), pch = 21, col = "#88c3b5", bg = "#66281b", xlab = "", xaxt = "n")
for(i in 1:length(m)) {
  # points(x = rep(i, length(m[[uid[i]]])), y = m[[uid[i]]], cex = .1, col = "#00000033")
}
mtext(species[uid], side = 1, at = uid, las = 2, line = 1)
points(x = seq(1, length(m)), y = means[uid], pch = 21, col = "#88c3b5", bg = "#66281b", cex = 2)


