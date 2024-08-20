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
new <- read.csv("trends/All_Shorebird_migration_survey_wide_trends.csv")
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

# NA's in Long-tailed Jaeger distribution ??
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
  # ggplot2::geom_text(ggplot2::aes(label = birds), vjust = -0.5) +
  ggplot2::labs(x = "Exposure Mean", y = "% Population Change (in 100's)", title = "Exposure mean of crop distribution vs. population trends")

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
summary.aov(model6)



# 
# q <- stars::read_stars("data/data-format/bird-transformed/King_Eider/bird-transformed_King_Eider-terrestrial_human_footprint_venter-103a233e-Pasture2009.tif")
# image(q)



# Rework data to get rid of negative values...
new_perc <- alldat[, 6] + 95
new_perc <- as.data.frame(new_perc)
alldat <- cbind(alldat, new_perc)

# ANOVA without "category"
model7 <- aov(perc ~ exposure_mean, data = alldat)
plot(model7)
report::report(model7)
summary(model7)
summary.aov(model7)

# Kruskal for ANOVA, without "category"
kruskal.test(perc ~ exposure_mean, data = alldat)

# GLM with transformed data

par(mar = c(2, 2, 2, 2))

model8 <- glm(new_perc ~ exposure_mean * category, data = alldat, family = "poisson")
model8
summary(model8)
report::report(model8)
plot(model8)

pscl::pR2(model8)['McFadden']

# Bayesian GLM with GLMM (transformed data)
model9 <- MCMCglmm::MCMCglmm(new_perc ~ exposure_mean * category, data = alldat)
plot(model9)
summary(model9)

# Bayesian GLM with GLMM (non-transformed data)
model10 <- MCMCglmm::MCMCglmm(perc ~ exposure_mean * category, data = alldat)
plot(model10)
summary(model10)

# # # # Bayesian (again) # # # #

# "Need a stronger prior" when including individual species (bird), but works
# fine when including just category and population trends ... let's see how it looks

model11 <- MCMCglmm::MCMCglmm(perc ~ 1, random = ~ category + exposure_mean, data = alldat)
summary(model11)

#Posterior distribution plots
par(mfrow = c(1,3))

hist(mcmc(model11$VCV)[,"category"])
hist(mcmc(model11$VCV)[,"exposure_mean"])

par(mfrow=c(1,1))

# Here we can see that the distribution of variance for Location and Species is pressed right up against zero.
# For a random effect to be significant, we want the tails to be well removed from zero.

# Assessing model convergence
plot(model11$Sol)

# Assessing model convergence with variance of random effects
plot(model11$VCV)

# Looking at this plot, it suggests we will need a stronger prior... also evident by the 
# error in the beginning, and the inability to include species as an effect

# # #

# # # # Bayesian (again, with transformed population percent changes) # # # #

# "Need a stronger prior" when including individual species, but works
# fine when including just category and population trends

model12 <- MCMCglmm::MCMCglmm(new_perc ~ 1, random = ~ category + exposure_mean, data = alldat)
summary(model12)

#Posterior distribution plots - assessing significance
par(mfrow = c(2,2))

hist(mcmc(model12$VCV)[,"category"])
hist(mcmc(model12$VCV)[,"exposure_mean"])

par(mfrow=c(1,1))

# Assessing model convergence
plot(model12$Sol)

# Assessing model convergence with variance of random effects
plot(model12$VCV)

alldatnewnew <- alldat[-4,]
alldatnewnew <- alldatnewnew[-11,]
hist(alldatnewnew$perc)
