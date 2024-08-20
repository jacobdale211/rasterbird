on.exit(sf::sf_use_s2(TRUE), add = TRUE)
sf::sf_use_s2(FALSE)

# FOR CUMULATIVE (MULTIPLE)
 cumul <- dir("output/cumulative", full.names = TRUE) |>
   lapply(stars::read_stars)

# cumul <- stars::read_stars("output/data-stressors/terrestrial_human_footprint_venter-103a233e-croplands2005.tif")
brange = sf::st_read("./data/data-raw/birdlife/birds_multistress/Bylot_non_breeding_range.shp")
species <- brange$species
pop <- read.csv("data/data-raw/birdlife/birdlife-trends.csv")
pop <-pop[c("species", "percent_change","category")]

# New population trends for shorebirds
new <- read.csv("trends/All_Shorebird_migration_survey_wide_trends.csv")
sbirds <- new[new$trend_type == "Long-term",]
sbirds$category <- "shorebird"

# Filter & join
filter <- sbirds[sbirds$species %in% species, ]
final <- filter[c("species", "percent_change","category")] 
r <- rbind(final, pop)

# l <- list()
# for(i in 1:nrow(brange)) {
#   l[[i]] <- cumul[brange[i, ]]
# }


# Format data
m <- list()
for(i in 1:length(cumul)) {
  dat <- as.data.frame(cumul[[i]])
  colnames(dat)[3] <- "data"
  dat <- dplyr::filter(dat, !is.na(data))
  m[[i]] <- dat$data
}

means <- lapply(m, mean, na.rm = TRUE) |> unlist()
sds <- lapply(m, sd, na.rm = TRUE) |> unlist()

# Get bird names
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
  r[,c("species","percent_change", "category")], 
  by = c("bird" = "species")
) |>
  na.omit()



# Make pop percentage values numeric and multiply by -1 to get proper values for trends
# (unnecessary because of population trend data change)
# dat$perc <- as.numeric(dat$perc)
# dat$perc <- (dat$perc * -1)
 birds <- dat$bird

# # Remove cackling goose
# test <- dat[-4,]
# test_birds <- test$bird

# 11 species
library(ggplot2)
ggplot(data = dat, aes(x = dat$exposure_mean, y = dat$percent_change, color = category)) +
  geom_point() +
  geom_text(aes(label = birds), vjust = -0.5) +
  labs(x = "Exposure Mean", y = "% Population Change (in 100's)", title = "Exposure mean vs. population trends")


model <- glm(dat$percent_change ~ dat$exposure_mean * dat$category)
model
par(mar = c(2, 2, 2, 2))
plot(model)
summary(model)


# Without cackling goose
ggplot(data = test, aes(x = test$exposure_mean, y = test$perc)) +
  geom_point() +
  geom_text(aes(label = test_birds), vjust = -0.5) +
  labs(x = "Exposure Mean", y = "% Population Change (in 100's)", title = "Exposure mean vs. population trends")

par(mar = c(12,2,2,2))
plot(x = dat$exposure_mean, y = dat$perc)

plot(x = dat$exposure_mean[-c(10,4)], y = dat$perc[-c(10,4)])

# pch = 21, col = "#88c3b5", bg = "#66281b", xlab = "", xaxt = "n"
# for(i in 1:length(m)) {
 # points(x = rep(i, length(m[[uid[i]]])), y = m[[uid[i]]], cex = .1, col = "#00000033")
# }
# mtext(species[uid], side = 1, at = uid, las = 2, line = 1)
# points(x = seq(1, length(m)), y = means[uid], pch = 21, col = "#88c3b5", bg = "#66281b", cex = 2)

means

m <- list()
for(i in 1:length(cumul)) {
  dat <- as.data.frame(cumul[[i]])
  colnames(dat)[3] <- "data"
  dat <- dplyr::filter(dat, !is.na(data))
  m[[i]] <- dat$data
}