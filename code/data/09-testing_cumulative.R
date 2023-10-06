on.exit(sf::sf_use_s2(TRUE), add = TRUE)
sf::sf_use_s2(FALSE)

# FOR SINGLE DRIVERS AT A TIME
cumul <- stars::read_stars("data/data-stressors/terrestrial_human_footprint_venter-103a233e-croplands2005.tif")

# v <- read.csv("data/data-format/v-matrix.csv")

# Loads all stressors 
# cumul <- dir("output/data-stressors", full.names = TRUE) |>
  # lapply(stars::read_stars)

brange = sf::st_read("./data/data-raw/birdlife/birds_multistress/Bylot_non_breeding_range.shp")
species <- brange$species
pop <- read.csv("data/data-raw/birdlife/birdlife-trends.csv")
pop <-pop[c("species", "percent_change","category")]

# New population trends for shorebirds
new <- read.csv("All_Shorebird_migration_survey_wide_trends.csv")
sbirds <- new[new$trend_type == "Long-term",]
sbirds$category <- "shorebird"

# Filter & join
filter <- sbirds[sbirds$species %in% species, ]
final <- filter[c("species", "percent_change","category")] 
r <- rbind(final, pop)

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
  r[,c("species","percent_change","category")], 
  by = c("bird" = "species")
) |>
  na.omit()

# Rework exposure mean with vulnerability matrix (tabled til vulnerability rework)
# v[,5] <- as.numeric(v[,5])
# alldat$exposure_mean <- as.numeric(alldat$exposure_mean)
# new <- (v[,5] * alldat$exposure_mean)
# new <- as.numeric(new)

# Fixes y axis - (change this to as.character to make graph clear, as.numeric to make y axis accurate)
alldat$perc <- as.numeric(alldat$perc)

# rdat <- range(alldat, na.rm = TRUE)
par(mar = c(20,20,20,20))

birds <- alldat$bird
ggplot2::ggplot(data = alldat, ggplot2::aes(x = alldat$exposure_mean, y = alldat$perc, color = category)) +
  ggplot2::geom_point() +
  ggplot2::geom_text(ggplot2::aes(label = birds), vjust = -0.5) +
  ggplot2::labs(x = "Exposure Mean", y = "% Population Change (in 100's)", title = "Exposure mean vs. population trends of Crop Distribution")

# GLM

par(mar = c(2, 2, 2, 2))

model <- glm(perc ~ exposure_mean * category, data = alldat, family = "poisson")
model
summary(model)
report::report(model)
plot(model)

pscl::pR2(model)['McFadden']

# ANOVA 

model2 <- aov(perc ~ category, data = alldat)
summary(model2)

report::report(model2)

# LM with random effects

model3 <- lme4::lmer(perc ~ exposure_mean * (1|category), data = alldat)

model3
summary(model3)
report::report(model3)
plot(model3)
abline(model3)

#  LM
model4 <- lm(perc ~ exposure_mean * category, data = alldat)
model4
summary(model4)
report::report(model4)
plot(model4)
abline(model4)

# Bayesian
model5 <- brms::brm(perc ~ exposure_mean * category, data = alldat)
plot(model5)
report::report(model5)

# ANOVA
model6 <- aov(perc ~ exposure_mean * category, data = alldat)
plot(model6)
report::report(model6)
summary(model6)
# 
# q <- stars::read_stars("data/data-format/bird-transformed/King_Eider/bird-transformed_King_Eider-terrestrial_human_footprint_venter-103a233e-Pasture2009.tif")
# image(q)
