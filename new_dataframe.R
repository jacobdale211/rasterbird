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
# DB: If you can, try not to use numbers in brackets like this, because if the order of your elements changes for whatever reason, you would then remove the wrong element. Ideally, you would use a name, or anything that would remain constant. 
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

centroid <- sf::st_centroid(amgplo_d)
centroid

# 2, Black-bellied_Plover
blbplo_u <- sf::st_union(p[[2]])
blbplo_d <- sf::st_cast(blbplo_u, "POLYGON")

centroid <- sf::st_centroid(blbplo_d[[4]])
centroid

# 3, Buff-breasted Sandpiper, 1 range
bubsan_u <- sf::st_union(p[[3]])
bubsan_d <- sf::st_cast(bubsan_u, "POLYGON")

centroid <- sf::st_centroid(bubsan_d)
centroid

# 4, Cackling Goose
cacgoo_u <- sf::st_union(p[[4]])
cacgoo_d <- sf::st_cast(cacgoo_u, "POLYGON")

centroid <- sf::st_centroid(cacgoo_d[[1]])
centroid

# 5, Glaucous Gull
glagul_u <- sf::st_union(p[[5]])
glagul_d <- sf::st_cast(glagul_u, "POLYGON")

centroid <- sf::st_centroid(glagul_d)
centroid

# 6, King Eider
kineid_u <- sf::st_union(p[[6]])
kineid_d <- sf::st_cast(kineid_u, "POLYGON")

centroid <- sf::st_centroid(kineid_d)
centroid

# 7, Long-tailed Duck
lotduc_u <- sf::st_union(p[[7]])
lotduc_d <- sf::st_cast(lotduc_u, "POLYGON")

centroid <- sf::st_centroid(lotduc_d[[9]])
centroid

# 8, Long-tailed Jaeger
lotjae_u <- sf::st_union(p[[8]])
lotjae_d <- sf::st_cast(lotjae_u, "POLYGON")

centroid <- sf::st_centroid(lotjae_d[[13]])
centroid

# 9, Parasitic Jaeger
parjae_u <- sf::st_union(p[[9]])
parjae_d <- sf::st_cast(parjae_u, "POLYGON")

centroid <- sf::st_centroid(parjae_d[[6]])
centroid

# 10, Pectoral Sandpiper
pecsan_u <- sf::st_union(p[[10]])
pecsan_d <- sf::st_cast(pecsan_u, "POLYGON")

centroid <- sf::st_centroid(pecsan_d)
centroid

# 11, Red Knot * need to remove EU and Africa populations
redkno_u <- sf::st_union(p[[11]])
redkno_d <- sf::st_cast(redkno_u, "POLYGON")

centroid <- sf::st_centroid(redkno_d)
centroid

# Removing EU and Africa polygons
redkno_d <- redkno_d[-3] # Run 3 times

# 12, Red-throated Loon, 1 range
retloo_u <- sf::st_union(p[[12]])
retloo_d <- sf::st_cast(retloo_u, "POLYGON")

centroid <- sf::st_centroid(retloo_d)
centroid

# 13, Ruddy Turnstone * need to remove EU and Africa populations
rudtur_u <- sf::st_union(p[[13]])
rudtur_d <- sf::st_cast(rudtur_u, "POLYGON")

centroid <- sf::st_centroid(rudtur_d)
centroid

# Removing EU and Africa polygons
rudtur_d <- rudtur_d[-2] # Run 5 times
# DB: What do you mean run five times? If I understand correctly, I would give the same comment as on line 17.

# 14, Snow Goose
snogoo_u <- sf::st_union(p[[14]])
snogoo_d <- sf::st_cast(snogoo_u, "POLYGON")

centroid <- sf::st_centroid(snogoo_d)
centroid

# 15, Tundra Swan, 1 range
tunswa_u <- sf::st_union(p[[15]])
tunswa_d <- sf::st_cast(tunswa_u, "POLYGON")

centroid <- sf::st_centroid(tunswa_d)
centroid

# 16, White-rumped Sandpiper
whrsan_u <- sf::st_union(p[[16]])
whrsan_d <- sf::st_cast(whrsan_u, "POLYGON")

centroid <- sf::st_centroid(whrsan_d)
centroid



# Extract values from rasters
drivers <- dir("data/data-stressors", full.names = TRUE) |>
  lapply(stars::read_stars)

list_birds <- c(amgplo_d, blbplo_d, bubsan_d, cacgoo_d, glagul_d, kineid_d, lotduc_d, lotjae_d, parjae_d,
                pecsan_d, redkno_d, retloo_d, rudtur_d, snogoo_d, tunswa_d, whrsan_d)

list_num_birds <- c(1, 4, 1, 2, 1, 3, 9, 13, 6, 2, 2, 1, 1, 2, 1, 3)
list_name_birds <- c("American_Golden-Plover", "Black-bellied_Plover", "Buff-breasted_Sandpiper", "Cackling_Goose", "Glaucous_Gull",
                     "King_Eider", "Long-tailed_Duck", "Long-tailed_Jaeger", "Parasitic_Jaeger", "Pectoral_Sandpiper", "Red_Knot",
                     "Red-throated_Loon", "Ruddy_Turnstone", "Snow_Goose", "Tundra_Swan", "White-rumped_Sandpiper")
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
final_df <- data.frame(species = rep(NA, 832),
                       mean = rep(NA, 832),
                       max = rep(NA, 832),
                       min = rep(NA, 832),
                       sd = rep(NA, 832))
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
      # !!!!!!!!!!!!!!!!!!
      # DB: Be careful, here you are using object names that are all functions, and functions that you are using on top of it. 
      #     This could be a source of error, because you are essentially replacing the function by an object. I actually wonder 
      #     whether it might not completely mess with your calculations. The first iterations should be ok, but it's unclear 
      #     whether the following iterations would be fine. 
      # !!!!!!!!!!!!!!!!!!
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

reformatted_data$max <- as.numeric(reformatted_data$max)

# Check distributions
hist(reformatted_data$mean) 
hist(reformatted_data$max)
hist(reformatted_data$min)
hist(reformatted_data$sd)

# Need to remove NA's
# DB: Are NAs present in all elements of a row or a column, or constrained to single cells? If they are constrained, you may be removing some data points unnecessarily.
final_df_filtered <- na.omit(reformatted_data)

# Recheck distributions (the same)
hist(final_df_filtered$mean) 
hist(final_df_filtered$max)
hist(final_df_filtered$min)
hist(final_df_filtered$sd)

# Population trend distributions (not accurate because of the way data is formatted in this DF)
hist(final_df_filtered$percent)
# Still lots of negatives!


# Reworking data frame to assign binomial values to trends
# (positve & non-increasing vs decreasing)
# DB: Does this need to be modified if non-increasing are removed?
final_df_filtered$perc_binomial <- ifelse(final_df_filtered$percent >= 0, 1, 0)


# Reformat species names to get rid of spaces to split first column
# NOTE: spaces have been removed in list_name_birds() object; no need to run this step if the dataframe
# is reproduced
# DB: I think a single line of command could have been enough?
#     final_df_filtered$species <- gsub(" ", "-", final_df_filtered$species)
final_df_filtered$species <- gsub("American Golden-Plover", "American_Golden-Plover", final_df_filtered$species)
final_df_filtered$species <- gsub("Buff-breasted Sandpiper", "Buff-breasted_Sandpiper", final_df_filtered$species)
final_df_filtered$species <- gsub("Cackling Goose", "Cackling_Goose", final_df_filtered$species)
final_df_filtered$species <- gsub("Glaucous Gull", "Glaucous_Gull", final_df_filtered$species)
final_df_filtered$species <- gsub("King Eider", "King_Eider", final_df_filtered$species)
final_df_filtered$species <- gsub("Long-tailed Duck", "Long-tailed_Duck", final_df_filtered$species)
final_df_filtered$species <- gsub("Long-tailed Jaeger", "Long-tailed_Jaeger", final_df_filtered$species)
final_df_filtered$species <- gsub("Parasitic Jaeger", "Parasitic_Jaeger", final_df_filtered$species)
final_df_filtered$species <- gsub("Pectoral Sandpiper", "Pectoral_Sandpiper", final_df_filtered$species)
final_df_filtered$species <- gsub("Red Knot", "Red_Knot", final_df_filtered$species)
final_df_filtered$species <- gsub("Red-throated Loon", "Red-throated_Loon", final_df_filtered$species)
final_df_filtered$species <- gsub("Ruddy Turnstone", "Ruddy_Turnstone", final_df_filtered$species)
final_df_filtered$species <- gsub("Snow Goose", "Snow_Goose", final_df_filtered$species)
final_df_filtered$species <- gsub("Tundra Swan", "Tundra_Swan", final_df_filtered$species)
final_df_filtered$species <- gsub("White-rumped Sandpiper", "White-rumped_Sandpiper", final_df_filtered$species)

# Split first column into 3 (species, range number, stressor)
final_df_filtered <- tidyr::separate(final_df_filtered, species, into = c("sp", "range", "stressor"), sep = " ")

# Pivot df to look at stressors individually
final_df_wide <- tidyr::pivot_wider(final_df_filtered, names_from = stressor, values_from = c(mean, max, min, sd))






# Models with individual stressors

#####
model <- glm(perc_binomial ~ mean_inorganic, data = final_df_wide, family = "binomial")
summary(model)
resid.model = resid(model)
plot(fitted(model), resid.model)
abline(0,0)
# Summary of model
plot(model)
summary(model)

# Obtaining odds of the slope (odds ratio) for explanatory variables (in this case, just mean exposure)
exp(model$coefficients[2])
# Confidence interval on the odds scale
exp(confint(model)[2,])
 
# Goodness-of-fit
# PseudoR2
objects(model)
pseudoR2 <- (model$null.deviance - model$deviance) / model$null.deviance
pseudoR2

glmtoolbox::hltest(model)

#####
model <- glm(perc_binomial ~ mean_invasives, data = final_df_wide, family = "binomial")
summary(model)
#####
model <- glm(perc_binomial ~ mean_lights, data = final_df_wide, family = "binomial")
summary(model)
#####
model <- glm(perc_binomial ~ mean_ocean_pollution, data = final_df_wide, family = "binomial")
summary(model)
#####
model <- glm(perc_binomial ~ mean_plumes_fert, data = final_df_wide, family = "binomial")
summary(model)
#####
model <- glm(perc_binomial ~ mean_plumes_pest, data = final_df_wide, family = "binomial")
summary(model)
#####
model <- glm(perc_binomial ~ mean_population, data = final_df_wide, family = "binomial")
summary(model)
#####
model <- glm(perc_binomial ~ mean_shipping, data = final_df_wide, family = "binomial")
summary(model)
#####
model <- glm(perc_binomial ~ mean_Built2009, data = final_df_wide, family = "binomial")
summary(model)
#####
model <- glm(perc_binomial ~ mean_croplands2005, data = final_df_wide, family = "binomial")
summary(model)
#####
model <- glm(perc_binomial ~ mean_NavWater2009, data = final_df_wide, family = "binomial")
summary(model)
#####
model <- glm(perc_binomial ~ mean_Lights2009, data = final_df_wide, family = "binomial")
summary(model)
#####
model <- glm(perc_binomial ~ mean_Pasture2009, data = final_df_wide, family = "binomial")
summary(model)
#####
model <- glm(perc_binomial ~ mean_Popdensity2010, data = final_df_wide, family = "binomial")
summary(model)
#####
model <- glm(perc_binomial ~ mean_Roads, data = final_df_wide, family = "binomial")
summary(model)
#####
model <- glm(perc_binomial ~ mean_Railways, data = final_df_wide, family = "binomial")
summary(model)



# Visualizing data before looking at GLMs of mean, max, min, sd
plot(perc_binomial ~ mean, data = final_df_filtered)
plot(percent ~ mean, data = final_df_filtered)
boxplot(mean ~ percent , data= final_df_filtered)

# mean, max, sd, min models (without species or categories)
######
# Let's try a model with means from all stressors & bird ranges
# Does not converge when using "sp" as random factor
model <- glm(perc_binomial ~ mean + (1|sp), data = final_df_filtered, family = "binomial")

# Plot of residuals
resid.model = resid(model)
plot(fitted(model), resid.model)
abline(0,0)
# Summary of model
plot(model)
summary(model)

# Obtaining odds of the slope (odds ratio) for explanatory variables 
# (odds of probability of success for each explanatory variable)

exp(model$coefficients[2])
# mean 
# 0.2970207 
# When the odds value is smaller than 1, interpretation is a little bit more complicated. 
# When this is the case, we have to take the inverse value (i.e. 1 divided by the odds) to facilitate 
# interpretation. The interpretation is then how LESS likely it is to observe the event of interest.


# Confidence interval on the odds scale
exp(confint(model)[2,])
# 2.5 %    97.5 % 
# 0.1109478 0.7845887 

# Goodness-of-fit
# PseudoR2
objects(model)
pseudoR2 <- (model$null.deviance - model$deviance) / model$null.deviance
pseudoR2
# 0.007862609 very low R2

# Hosmer-Lemeshow test... evaluates whether the logistic regression model is a good fit for the data
glmtoolbox::hltest(model)
# p-value =  2.6391e-05 very low p-value, indicative of a poor fit

# Plotting model
ggplot(final_df_filtered, aes(x = mean, y = perc_binomial)) + geom_point() + 
  stat_smooth(method = "glm", family= "binomial", se = FALSE) + xlab("Mean exposure") +
  ylab("Populastion trends") + 
  ggtitle("Binomial population trends as a function of mean exposure")

######

# Let's try a model with max values now 
# Does not converge when using "sp" as random factor
model <- glm(perc_binomial ~ max, data = final_df_filtered, family = "binomial")

# Plot of residuals
resid.model = resid(model)
plot(fitted(model), resid.model)
abline(0,0)
# Summary of model
plot(model)
summary(model)

# Obtaining odds of the slope (odds ratio) for explanatory variables (in this case, just mean exposure)
exp(model$coefficients[2])
# max 
# 0.2134073
# Confidence interval on the odds scale

exp(confint(model)[2,])
# 2.5 %    97.5 % 
# 0.1330352 0.3384291  

# Goodness-of-fit
# PseudoR2
objects(model)
pseudoR2 <- (model$null.deviance - model$deviance) / model$null.deviance
pseudoR2
# 0.05865134 very low R2

# Hosmer-Lemeshow test... evaluates whether the logistic regression model is a good fit for the data
glmtoolbox::hltest(model)
# p-value =  0.43637, much higher than means

######
# standard deviation
model <- glm(perc_binomial ~ sd, data = final_df_filtered, family = "binomial")

# Plot of residuals
resid.model = resid(model)
plot(fitted(model), resid.model)
abline(0,0)
# Summary of model
plot(model)
summary(model)

# Obtaining odds of the slope (odds ratio) for explanatory variables
# (odds of probability of success for each explanatory variable)

exp(model$coefficients[2])
# sd 
# 0.002636511 
# Confidence interval on the odds scale

exp(confint(model)[2,])
# 2.5 %    97.5 % 
# 0.0002972836 0.0212812842   

# Goodness-of-fit
# PseudoR2
objects(model)
pseudoR2 <- (model$null.deviance - model$deviance) / model$null.deviance
pseudoR2
# 0.04316169 very low R2

# Hosmer-Lemeshow test... evaluates whether the logistic regression model is a good fit for the data
glmtoolbox::hltest(model)
# p-value = 0.0034068

######
model <- glm(perc_binomial ~ min, data = final_df_filtered, family = "binomial")

# Plot of residuals
resid.model = resid(model)
plot(fitted(model), resid.model)
abline(0,0)
# Summary of model
plot(model)
summary(model)

# Obtaining odds of the slope (odds ratio) for explanatory variables (in this case, just mean exposure)
# (odds of probability of success for each explanatory variable)

exp(model$coefficients[2])
# min 
# 0.984218 

# Confidence interval on the odds scale
exp(confint(model)[2,])
# 2.5 %    97.5 % 
# 0.3244942 2.9814796 

# Goodness-of-fit
# PseudoR2
objects(model)
pseudoR2 <- (model$null.deviance - model$deviance) / model$null.deviance
pseudoR2
# 1.035565e-06 very low R2

# Hosmer-Lemeshow test... evaluates whether the logistic regression model is a good fit for the data
glmtoolbox::hltest(model)
# p-value = 0.75784 high p-value, better fit.. likely because of lots of 0's















##############################
# Let's try a model with just one species, with multiple ranges; Long tailed Duck
a <- "Long-tailed_Duck"
lotduc <- subset(final_df_filtered, sp == a)

model_lotduc <- glm(perc_binomial ~ mean, data = lotduc, family = "binomial")
summary(model_lotduc)

resid.lotduc <- resid(model_lotduc)

plot(fitted(model_lotduc), resid.lotduc)
abline(0,0)



# # "Reworking" of population trends, adding a constant value so all values are above 0
# # and a Poisson could be possible
# new_perc <- final_df_filtered[, 6] + 95
# new_perc <- as.data.frame(new_perc)
# final_df_filtered <- cbind(final_df_filtered, new_perc)
# 
# # ANOVA (?)
# # Estimates how a quantitative dependent variable changes 
# # according to the levels of one or more categorical independent variables.
# # In this case, how are population trends of certain species changing based on 
# # varying levels of exposure to drivers?
# 
# model_a <- aov(new_perc ~ mean * sp, data = final_df_filtered)
# plot(model_a)
# summary(model_a)






#### GLMER explorations

# Remove species with 0 population trends (for binomial approach)
binomial_df <- subset(final_df_filtered, subset = percent !='0')
# With wide dataset too
binomial_df_wide <- tidyr::pivot_wider(binomial_df, names_from = stressor, values_from = c(mean, max, min, sd))
# Now only 10 species, 22 ranges total

# stressor specific models
#####

#####
model <- lme4::glmer(perc_binomial ~ mean_invasives + (1|sp), data = binomial_df_wide, family = "binomial")
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ mean_lights + (1|sp), data = binomial_df_wide, family = "binomial")
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ mean_ocean_pollution + (1|sp), data = binomial_df_wide, family = "binomial")
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ mean_plumes_fert + (1|sp), data = binomial_df_wide, family = "binomial")
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ mean_plumes_pest + (1|sp), data = binomial_df_wide, family = "binomial")
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ mean_population + (1|sp), data = binomial_df_wide, family = "binomial")
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ mean_shipping + (1|sp), data = binomial_df_wide, family = "binomial")
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ mean_Built2009 + (1|sp), data = binomial_df_wide, family = "binomial")
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ mean_croplands2005 + (1|sp), data = binomial_df_wide, family = "binomial")
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ mean_NavWater2009 + (1|sp), data = binomial_df_wide, family = "binomial")
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ mean_Lights2009 + (1|sp), data = binomial_df_wide, family = "binomial")
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ mean_Pasture2009 + (1|sp), data = binomial_df_wide, family = "binomial")
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ mean_Popdensity2010 + (1|sp), data = binomial_df_wide, family = "binomial")
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ mean_Roads + (1|sp), data = binomial_df_wide, family = "binomial")
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ mean_Railways + (1|sp), data = binomial_df_wide, family = "binomial")
summary(model)



#####

# GLM's with removed 0 trend species and means, mins, maxes, and sd's for 

# mean
model <- lme4::glmer(perc_binomial ~ mean + (1|sp), data = binomial_df, family = "binomial")
summary(model)
# Plot of residuals vs. fitted
plot(model)

# max
model <- lme4::glmer(perc_binomial ~ max + (1|sp), data = binomial_df, family = "binomial")
summary(model)
# Plot of residuals vs. fitted
plot(model)

# min
model <- lme4::glmer(perc_binomial ~ min + (1|sp), data = binomial_df, family = "binomial")
summary(model)
# Plot of residuals vs. fitted
plot(model)

# sd
model <- lme4::glmer(perc_binomial ~ sd + (1|sp), data = binomial_df, family = "binomial")
summary(model)
# Plot of residuals vs. fitted
plot(model)




# glmer.nb (trying something, didn't work)

# mean
model <- lme4::glmer.nb(perc_binomial ~ mean + (1|sp), data = binomial_df, family = "binomial")
summary(model)
# Plot of residuals vs. fitted
plot(model)

# max
model <- lme4::glmer.nb(perc_binomial ~ max + (1|sp), data = binomial_df, family = "binomial")
summary(model)
# Plot of residuals vs. fitted
plot(model)

# min
model <- lme4::glmer.nb(perc_binomial ~ min + (1|sp), data = binomial_df, family = "binomial")
summary(model)
# Plot of residuals vs. fitted
plot(model)

# sd
model <- lme4::glmer.nb(perc_binomial ~ sd + (1|sp), data = binomial_df, family = "binomial")
summary(model)
# Plot of residuals vs. fitted
plot(model)


