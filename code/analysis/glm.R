###


res <- read.csv("res_terra_edit_moresp.csv")
# Remove extra columns that pop up after exporting too many times
library(dplyr)
library(tidyr)
library(visreg)


s_t_r <- c("x")
res <- res %>%
  select(-contains(s_t_r))





# # pivot_longer for histograms
# test <- res
# s_t_r <- c("coverage_fraction","range_size", "x", "y")
# test <- test %>%
#   select(-contains(s_t_r))
# 
# df_long <- test %>%
#   pivot_longer(cols = -c(sp, range, perc_binomial, range_scale, time), names_to = "stressor", values_to = "value")
# 
# # all means & maxes
# df_stats <- df_long %>%
#   group_by(sp, range, stressor, perc_binomial, range_scale, time) %>%
#   summarize(
#     mean_value = mean(value, na.rm = TRUE),
#     min_value = ifelse(all(is.na(value)), NA, min(value, na.rm = TRUE)),  # Check for all NA values
#     max_value = ifelse(all(is.na(value)), NA, max(value, na.rm = TRUE)),  # Check for all NA values
#     sd_value = sd(value, na.rm = TRUE)
#   ) %>%
#   ungroup()
# 
# # individualized vals
# df_final <- df_stats %>%
#   pivot_wider(
#     names_from = stressor,
#     values_from = c(mean_value, max_value, sd_value, min_value),
#     names_sep = "_"
#   )
# # model <- lme4::glmer(perc_binomial ~  mean_value_lights_halp  + range_scale + (1|sp), data = df_final, family = "binomial", weights = time)
# # summary(model)
# ggplot(df_stats, aes(x = sd_value)) +
#   geom_histogram(bins = 20, alpha = 0.7, color = "black", fill = "#69b3a2") +
#   labs(
#     x = "Min Value",
#     y = "Frequency",
#     title = "Histogram of SD Values"
#   ) +
#   theme_minimal()

# res$perc_binomial <- as.numeric(res$perc_binomial)
# stat function
calculate_stats <- function(data, value_column, group_column1, group_column2, group_column3, 
                            group_column4, group_column5) {
  stats <- data %>%
    drop_na({{value_column}}) %>%
    group_by({{group_column1}}, {{group_column2}}, {{group_column3}}, 
             {{group_column4}}, {{group_column5}}) %>%
    summarise(mean = mean({{value_column}}, na.rm = TRUE),
              min = min({{value_column}}, na.rm = TRUE),
              max = max({{value_column}}, na.rm = TRUE),
              sd = sd({{value_column}}, na.rm = TRUE))
  
  return(stats)
}

### inorganic
result_inorganic <- calculate_stats(res, value_column = inorganic, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  max  + range_scale + (1|sp), data = result_inorganic, family = "binomial", weights = time)
summary(model)
hist(result_inorganic$max)
scatter.smooth(result$mean, result$perc_binomial, main = "Inorganic pollution")
performance::r2(model)
visreg(model)


### invasives
result <- calculate_stats(res, value_column = invasives, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Invasive species presence")
performance::r2(model)
visreg(model)

### lights, halpern
result_lights_halp <- calculate_stats(res, value_column = lights_halp, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Light pollution (Halpern)")
performance::r2(model)
visreg(model)

### ocean pollution
result_ocn_pol <- calculate_stats(res, value_column = ocn_pol, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Ocean Pollution")
performance::r2(model)
visreg(model)

### fertilizer plumes
result_plm_fert <- calculate_stats(res, value_column = plm_fert, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Fertilizer plumes")
performance::r2(model)
visreg(model)

### pesticide plumes
result_plm_pest <- calculate_stats(res, value_column = plm_pest, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Pesticide Plumes")
performance::r2(model)
visreg(model)

### population, halpern
result_pop_halp <- calculate_stats(res, value_column = pop_halp, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Population density (Halpern)")
performance::r2(model)
visreg(model)

### shipping
result_ship <- calculate_stats(res, value_column = ship, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Shipping")
performance::r2(model)
visreg(model)


###venter


### urban environments
result_built <- calculate_stats(res, value_column = built, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Urban Environments")
performance::r2(model)
visreg(model)

### croplands
result_croplands <- calculate_stats(res, value_column = croplands, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Cropland distribution")
performance::r2(model)
visreg(model)

### lights, venter
result_lights_vent <- calculate_stats(res, value_column = lights_vent, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Light pollution (Venter)")
performance::r2(model)
visreg(model)

### waterways
result_navwater <- calculate_stats(res, value_column = navwater, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Waterway distribution")
performance::r2(model)
visreg(model)

### pastures
result_pastures <- calculate_stats(res, value_column = pastures, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Pasture distribution")
performance::r2(model)
visreg(model)

### population, venter
result_pop_vent <- calculate_stats(res, value_column = pop_vent, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Population density (Venter)")
performance::r2(model)
visreg(model)

### railways
result_railways <- calculate_stats(res, value_column = railways, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Railway distribution")
performance::r2(model)
visreg(model)

### roads
result_roads <- calculate_stats(res, value_column = roads, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Road distribution")
performance::r2(model)
visreg(model)







### plots
library(sjPlot)
pp <- sjPlot::plot_model(model, type ="pred",
                         terms = c("max [all]", "range_scale"), 
                         pred.type="re", 
                         axis.title = c("Urban sprawl Venter","Probability"), 
                         legend.title = "Range size (scaled)", 
                         title= "Predicted Probabilities of Positive Population Trends", show.legend = TRUE)
plot(pp)

# residuals
pp_r <- sjPlot::plot_model(model, type ="resid",
                           terms = c("max", "range_scale"), 
                           pred.type="re", 
                           axis.title = c("driver","Probability"), 
                           legend.title = "Range size (scaled)", 
                           title= "Predicted Probabilities of Positive Population Trends", show.legend = TRUE)
plot(pp_r)
sjPlot::plot_model(model, type = "resid", vline.color = "red")

sjPlot::plot_model(model)

### r-squared
performance::r2(model)
#####
#####outakes
# 
# # 1 deg buffer
# shp_buf <- st_buffer(shp, dist = 1)

# Specify desired bird ranges here 
# indices <- c(1, 3, 5, 6, 10, 12, 16, 18, 20, 22, 25, 27, 28)
# # Get bird ranges 
# shp <- shp[indices,]
# 


### inorganic
result <- calculate_stats(res, value_column = inorganic, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + (1|sp), data = result, family = "binomial")
summary(model)
performance::r2(model)


### invasives
result <- calculate_stats(res, value_column = invasives, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean+ (1|sp), data = result, family = "binomial" )
summary(model)
performance::r2(model)

### lights, halpern
result <- calculate_stats(res, value_column = lights_halp, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean   + (1|sp), data = result, family = "binomial" )
summary(model)
performance::r2(model)

### ocean pollution
result <- calculate_stats(res, value_column = ocn_pol, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean   + (1|sp), data = result, family = "binomial" )
summary(model)

### fertilizer plumes
result <- calculate_stats(res, value_column = plm_fert, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean   + (1|sp), data = result, family = "binomial" )
summary(model)

### pesticide plumes
result <- calculate_stats(res, value_column = plm_pest, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean   + (1|sp), data = result, family = "binomial" )
summary(model)

### population, halpern
result <- calculate_stats(res, value_column = pop_halp, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  max   + (1|sp), data = result, family = "binomial" )
summary(model)

### shipping
result <- calculate_stats(res, value_column = ship, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean   + (1|sp), data = result, family = "binomial" )
summary(model)


###venter


### urban environments
result <- calculate_stats(res, value_column = built, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean   + (1|sp), data = result, family = "binomial" )
summary(model)

### croplands
result <- calculate_stats(res, value_column = croplands, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean   + (1|sp), data = result, family = "binomial" )
summary(model)

### lights, venter
result <- calculate_stats(res, value_column = lights_vent, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  max   + (1|sp), data = result, family = "binomial" )
summary(model)

### waterways
result <- calculate_stats(res, value_column = navwater, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean   + (1|sp), data = result, family = "binomial" )
summary(model)

### pastures
result <- calculate_stats(res, value_column = pastures, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean   + (1|sp), data = result, family = "binomial" )
summary(model)

### population, venter
result <- calculate_stats(res, value_column = pop_vent, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean   + (1|sp), data = result, family = "binomial" )
summary(model)

### railways
result <- calculate_stats(res, value_column = railways, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean   + (1|sp), data = result, family = "binomial" )
summary(model)

### roads
result <- calculate_stats(res, value_column = roads, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean   + (1|sp), data = result, family = "binomial" )
summary(model)







######

### inorganic
result <- calculate_stats(res, value_column = inorganic, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lm(perc_binomial ~  min   , data = result, family = "binomial")
summary(model)
plot(result$min, result$perc_binomial, main = "Inorganic pollution")
visreg(model)

### invasives
result <- calculate_stats(res, value_column = invasives, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lm(perc_binomial ~  min , data = result, family = "binomial" )
summary(model)
plot(result$min, result$perc_binomial, main = "Invasive species")

### lights, halpern
result <- calculate_stats(res, value_column = lights_halp, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lm(perc_binomial ~  min    , data = result)
summary(model)
plot(result$min, result$perc_binomial, main = "Light polltuion (Halpern)")

### ocean pollution
result <- calculate_stats(res, value_column = ocn_pol, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lm(perc_binomial ~  min    , data = result, family = "binomial" )
summary(model)
plot(result$min, result$perc_binomial, main = "Ocean pollution")


### fertilizer plumes
result <- calculate_stats(res, value_column = plm_fert, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lm(perc_binomial ~  min    , data = result, family = "binomial" )
summary(model)
plot(result$min, result$perc_binomial, main = "Fertilizer plumes")

### pesticide plumes
result <- calculate_stats(res, value_column = plm_pest, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lm(perc_binomial ~  min    , data = result, family = "binomial" )
summary(model)
plot(result$min, result$perc_binomial, main = "Pesticide plumes")


### population, halpern
result <- calculate_stats(res, value_column = pop_halp, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lm(perc_binomial ~  min    , data = result, family = "binomial" )
summary(model)
plot(result$min, result$perc_binomial, main = "Population density (Haleprn)")


### shipping
result <- calculate_stats(res, value_column = ship, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lm(perc_binomial ~  min    , data = result, family = "binomial" )
summary(model)
plot(result$min, result$perc_binomial, main = "Shipping")



###venter


### urban environments
result <- calculate_stats(res, value_column = built, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lm(perc_binomial ~  min    , data = result, family = "binomial" )
summary(model)
plot(result$min, result$perc_binomial, main = "Urban environments")


### croplands
result <- calculate_stats(res, value_column = croplands, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lm(perc_binomial ~  min    , data = result, family = "binomial" )
summary(model)
plot(result$max, result$perc_binomial, main = "Cropland distribution")

### lights, venter
result <- calculate_stats(res, value_column = lights_vent, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lm(perc_binomial ~  min    , data = result, family = "binomial" )
summary(model)
plot(result$max, result$perc_binomial, main = "Light pollution (Venter)")


### waterways
result <- calculate_stats(res, value_column = navwater, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lm(perc_binomial ~  min    , data = result, family = "binomial" )
summary(model)
plot(result$min, result$perc_binomial, main = "Waterway distribution")


### pastures
result <- calculate_stats(res, value_column = pastures, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lm(perc_binomial ~  mean    , data = result, family = "binomial" )
summary(model)
plot(result$min, result$perc_binomial, main = "Pasture distribution")


### population, venter
result <- calculate_stats(res, value_column = pop_vent, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lm(perc_binomial ~  min    , data = result, family = "binomial" )
summary(model)
plot(result$min, result$perc_binomial, main = "Population density (Venter)")

### railways
result <- calculate_stats(res, value_column = railways, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lm(perc_binomial ~  min    , data = result, family = "binomial" )
summary(model)
plot(result$min, result$perc_binomial, main = "Railways")

### roads
result <- calculate_stats(res, value_column = roads, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lm(perc_binomial ~  min   , data = result, family = "binomial" )
summary(model)
plot(result$min, result$perc_binomial, main = "Road distribution")




### RAW DATA
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

data <- read.csv("res_terra_edit_moresp.csv")

# List of environmental stressors
stressors <- c("inorganic", "invasives", "lights_halp", "ocn_pol", "plm_fert", 
               "plm_pest", "pop_halp", "ship", "built", "croplands", "lights_vent", 
               "navwater", "pastures", "pop_vent", "railways", "roads")

lm_stats <- function(x, y, species, stressor) {
  # Remove NA values
  valid <- complete.cases(x, y)
  x <- x[valid]
  y <- y[valid]
  
  cat("\nSpecies:", species, "| Stressor:", stressor, "\n")
  cat("Number of non-NA observations:", length(x), "\n")
  cat("Unique values in stressor:", length(unique(x)), "\n")
  cat("Unique values in percent trend:", length(unique(y)), "\n")
  cat("Range of stressor values:", range(x), "\n")
  cat("Range of percent trend values:", range(y), "\n")
  
  # Check if there's enough variation in x and y
  if (length(unique(x)) < 2 || length(unique(y)) < 2) {
    return(c(p_value = NA, r_squared = NA, reason = "Insufficient variation"))
  }
  
  # Perform regression
  model <- tryCatch({
    lm(y ~ x)
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(model)) {
    return(c(p_value = NA, r_squared = NA, reason = "Model fit failed"))
  }
  
  summary_model <- summary(model)
  
  p_value <- summary_model$coefficients[2, 4]
  r_squared <- summary_model$r.squared
  
  return(c(p_value = p_value, r_squared = r_squared, reason = "Success"))
}

results <- data.frame(species = character(),
                      stressor = character(),
                      p_value = numeric(),
                      r_squared = numeric(),
                      reason = character(),
                      stringsAsFactors = FALSE)

# Loop through each unique species
for (species in unique(data$sp)) {
  species_data <- subset(data, sp == species)
  
  # Loop through each stressor
  for (stressor in stressors) {
    # Check if the stressor column exists
    if (!(stressor %in% names(species_data))) {
      next
    }
    
    # Perform linear regression
    stats <- lm_stats(species_data[[stressor]], species_data$perc, species, stressor)
    
    # Add results to the dataframe
    results <- rbind(results, data.frame(
      species = species,
      stressor = stressor,
      p_value = as.numeric(stats["p_value"]),
      r_squared = as.numeric(stats["r_squared"]),
      reason = stats["reason"],
      stringsAsFactors = FALSE
    ))
    
    # Break after processing one species (for brevity in output)
    if (nrow(results) >= 16) break
  }
  if (nrow(results) >= 16) break
}

# View the results
print(results)

# Print summary of reasons
print(table(results$reason))



# MEANS
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



library(tidyr)
library(dplyr)
library(broom)

# Read the CSV file
data <- read.csv("sumsts.csv")

# Add percent trend data
percent_trends <- c(
  "American_Golden-Plover" = -73.31,
  "Bairds_Sandpiper" = -5.28,
  "Black-bellied_Plover" = -57.39,
  "Buff-breasted_Sandpiper" = -58.17,
  "Cackling_Goose" = 833.13,
  "Glaucous_Gull" = 0,
  "King_Eider" = -83.34,
  "Long-tailed_Duck" = -65.04,
  "Long-tailed_Jaeger" = 0,
  "Pacific_Loon" = 0,
  "Parasitic_Jaeger" = 0,
  "Pectoral_Sandpiper" = -64.40,
  "Red_Knot" = -94.05,
  "Red-throated_Loon" = 21.18,
  "Ruddy_Turnstone" = -76.29,
  "Snow_Goose" = 477.0,
  "Tundra_Swan" = 3.99,
  "White-rumped_Sandpiper" = -26.57
)

# Reshape the data
reshaped_data <- data %>%
  select(sp, driver, mean) %>%
  pivot_wider(names_from = driver, values_from = mean) %>%
  mutate(perc = percent_trends[sp])

# Remove any columns that are all NA
reshaped_data <- reshaped_data %>% select_if(~!all(is.na(.)))

# Get the names of the driver columns
driver_cols <- setdiff(names(reshaped_data), c("sp", "perc"))

# Function to perform linear regression and return results
lm_stats <- function(data, species) {
  model <- lm(perc ~ ., data = data[, c(driver_cols, "perc")])
  summary_model <- summary(model)
  
  # Extract coefficients
  coef_data <- tidy(model)
  
  # Prepare results
  results <- data.frame(
    species = species,
    r_squared = summary_model$r.squared,
    adj_r_squared = summary_model$adj.r.squared
  )
  
  # Add coefficient data
  for(i in 1:nrow(coef_data)) {
    results[[paste0(coef_data$term[i], "_estimate")]] <- coef_data$estimate[i]
    results[[paste0(coef_data$term[i], "_p_value")]] <- coef_data$p.value[i]
  }
  
  return(results)
}

# Initialize an empty list to store results
results_list <- list()

# Loop through each species
for (species in unique(reshaped_data$sp)) {
  species_data <- reshaped_data[reshaped_data$sp == species, ]
  
  # Check if we have enough data points
  if (nrow(species_data) > 1) {
    # Perform linear regression
    tryCatch({
      results_list[[species]] <- lm_stats(species_data, species)
    }, error = function(e) {
      cat("Error processing species:", species, "\n")
      print(e)
    })
  } else {
    cat("Not enough data points for species:", species, "\n")
  }
}

# Combine all results
results <- do.call(rbind, results_list)

# Print results
print(results)

# Optionally, save the results to a CSV file
write.csv(results, "lm_results_by_species.csv", row.names = FALSE)




# LOOKING AT SINGULAR MODELS
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


# testing more things
data <- read.csv("res_terra_edit_moresp.csv")
cgoo <- data[grepl("Cackling_Goose", data$sp), ]

modeltest <- lm(perc ~ inorganic, data = cgoo)
summary(modeltest)

modeltest <- lm(perc ~ croplands, data = cgoo)
summary(modeltest)

# and some more
sumsts <- read.csv("sumsts.csv")
sumsts <- sumsts %>%
  mutate(perc = case_when(
    sp == "American_Golden-Plover" ~ "-73.31",
    sp == "Bairds_Sandpiper" ~ "-5.28",
    sp == "Black-bellied_Plover" ~ "-57.39",
    sp == "Buff-breasted_Sandpiper" ~ "-58.17",
    sp == "Cackling_Goose" ~ "833.13",
    sp == "Glaucous_Gull" ~ "0",
    sp == "King_Eider" ~ "-83.34",
    sp == "Long-tailed_Duck" ~ "-65.04",
    sp == "Long-tailed_Jaeger" ~ "0",
    sp == "Pacific_Loon" ~ "0",
    sp == "Parasitic_Jaeger" ~ "0",
    sp == "Pectoral_Sandpiper" ~ "-64.40",
    sp == "Red_Knot" ~ "-94.05",
    sp == "Red-throated_Loon" ~ "21.18",
    sp == "Ruddy_Turnstone" ~ "-76.29",
    sp == "Snow_Goose" ~ "477.0",
    sp == "Tundra_Swan" ~ "3.99",
    sp == "White-rumped_Sandpiper" ~ "-26.57"
  ))
cgoo <- sumsts[grepl("Cackling_Goose", sumsts$sp), ]
modeltest2 <- lm(perc ~ mean, data = cgoo)
summary(modeltest2)
#Warning message:
#In summary.lm(modeltest2) :
#  essentially perfect fit: summary may be unreliable








#### ARCHIVE GLM SCRIPTS, TRIPLE NESTED LOOP 2023

# Get drivers
drivers <- dir("data/data-stressors", full.names = TRUE) |>
  lapply(stars::read_stars)

# 
# # messing around
# test <- sf::st_as_sf(drivers[[1]])
# test <- sf::st_set_crs(test, 4326)
# test2 <- sf::st_sfc(sf::st_polygon(amgplo_d[[1]]), crs = 4326)
# intersection <- sf::st_intersects(test2, test)
# 
# install.packages("raster")
# install.packages("exactextractr")


# removed glagul_d, lotjae_d, parjae_d
list_birds <- c(amgplo_d, blbplo_d, bubsan_d, cacgoo_d, kineid_d, lotduc_d,
                pecsan_d, redkno_d, retloo_d, rudtur_d, snogoo_d, tunswa_d, whrsan_d)

list_num_birds <- c(1, 7, 1, 3, 3, 12, 2, 3, 1, 1, 2, 1, 4)
list_name_birds <- c("American_Golden-Plover", "Black-bellied_Plover", "Buff-breasted_Sandpiper", "Cackling_Goose",
                     "King_Eider", "Long-tailed_Duck", "Pectoral_Sandpiper", "Red_Knot",
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

# iterator <- 1
#  i=1
#  j=0
#  k=1

##### Aggregate loop (functional)

# for (i in 1:length(list_num_birds)) {
#   current_bird <- list_birds[[i]]
#   current_num <- list_num_birds[[i]]
#   
#   # loop for each range per bird
#   for (j in 0:(current_num - 1)) {
#     bird_range <- sf::st_sfc(sf::st_polygon(list_birds[[i + j]]), crs = 4326)
#     # loop for each driver for each range
#     for (k in 1:length(drivers)) {
#       dat <- (drivers[[k]])
#       
#       a_mean <- raster::aggregate(drivers[[k]], bird_range, FUN = mean, na.rm = TRUE)
#       a_max <- raster::aggregate(drivers[[k]], bird_range, FUN = max, na.rm = TRUE)
#       a_min <- raster::aggregate(drivers[[k]], bird_range, FUN = min, na.rm = TRUE)
#       a_sd <- raster::aggregate(drivers[[k]], bird_range, FUN = sd, na.rm = TRUE)
#       
#       b <- as.numeric(a_mean)
#       c <- as.numeric(a_max)
#       d <- as.numeric(a_min)
#       e <- as.numeric(a_sd)
#       
#       # Add the calculated values to the data frame within the loop
#       final_df[iterator, "species"] <- paste(list_name_birds[[i]],j+1,list_name_drivers[[k]], sep = " ")
#       final_df[iterator, "mean"] <- b
#       final_df[iterator, "max"] <- c
#       final_df[iterator, "min"] <- d
#       final_df[iterator, "sd"]  <- e
#       iterator = iterator + 1
#       print(iterator)
#     }
#   }
#   print(list_name_birds[[i]])
#   i <- i + current_num
# }


iterator <- 1
i=1
j=0
k=1

##### Loop that doesn't work :(
# Line 230 is the line that causes the problem; line 234 runs indefinitely 

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
      
      
      c <- mean(a[[1]], na.rm = TRUE) 
      d <- max(a[[1]], na.rm = TRUE)
      e <- min(a[[1]], na.rm = TRUE)
      f <- sd(a[[1]], na.rm = TRUE)
      
      # Add the calculated values to the data frame within the loop
      final_df[iterator, "species"] <- paste(list_name_birds[[i]],j+1,list_name_drivers[[k]], sep = " ")
      final_df[iterator, "mean"] <- c
      final_df[iterator, "max"] <- d
      final_df[iterator, "min"] <- e
      final_df[iterator, "sd"]  <- f
      iterator = iterator + 1
      print(iterator)
    }
  }
  print(list_name_birds[[i]])
  i <- i + current_num
}

# Export dataframe
write.csv(final_df, file = "name.csv", row.names = FALSE)

# Read in csv (choose your fighter)

# OG data 
reformatted_data <- read.csv("csv/final_df_original.csv") 
# Aggregate data
reformatted_data <- read.csv("final_df_agg.csv")
# No CRS assignment data
reformatted_data <- read.csv("final_df_nc.csv")

reformatted_data$max <- as.numeric(reformatted_data$max)

# Check distributions
# hist(reformatted_data$mean) 
# hist(reformatted_data$max)
# hist(reformatted_data$min)
# hist(reformatted_data$sd)

# Need to remove NA's
# DB: Are NAs present in all elements of a row or a column, or constrained to single cells? 
# If they are constrained, you may be removing some data points unnecessarily.
# JD: All NA's were taking up the entirety of rows; no constrained cells were removed
final_df_filtered <- na.omit(reformatted_data)


# # Recheck distributions (the same)
# hist(final_df_filtered$mean) 
# hist(final_df_filtered$max)
# hist(final_df_filtered$min)
# hist(final_df_filtered$sd)
# 
# # Population trend distributions (not accurate because of the way data is formatted in this DF)
# hist(final_df_filtered$percent)
# # Still lots of negatives!


# Reworking data frame to assign binomial values to trends
# (positve & non-increasing vs decreasing)
# DB: Does this need to be modified if non-increasing are removed?
# JD: No, but this was added before we decided to remove non-increasing; marine and non-increasing birds are still
# part of the dataset
final_df_filtered$perc_binomial <- ifelse(final_df_filtered$percent >= 0, 1, 0)


# Reformat species names to get rid of spaces to split first column
# NOTE: spaces have been removed in list_name_birds() object; no need to run this step if the dataframe
# is reproduced
# DB: I think a single line of command could have been enough?
# Yes... 
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





# Old models, disregard. Statistical exploration starts at line 618
# Models with individual stressors
# 
# #####
# model <- glm(perc_binomial ~ mean_inorganic, data = final_df_wide, family = "binomial")
# summary(model)
# resid.model = resid(model)
# plot(fitted(model), resid.model)
# abline(0,0)
# # Summary of model
# plot(model)
# summary(model)
# 
# # Obtaining odds of the slope (odds ratio) for explanatory variables (in this case, just mean exposure)
# exp(model$coefficients[2])
# # Confidence interval on the odds scale
# exp(confint(model)[2,])
#  
# # Goodness-of-fit
# # PseudoR2
# objects(model)
# pseudoR2 <- (model$null.deviance - model$deviance) / model$null.deviance
# pseudoR2
# 
# glmtoolbox::hltest(model)
# 
# model <- glm(perc_binomial ~ max_inorganic, data = binomial_df_wide, family = "binomial")
# summary(model)
# #####
# model <- glm(perc_binomial ~ max_invasives, data = binomial_df_wide, family = "binomial")
# summary(model)
# #####
# model <- glm(perc_binomial ~ max_lights, data = binomial_df_wide, family = "binomial")
# summary(model)
# #####
# model <- glm(perc_binomial ~ max_ocean_pollution, data = binomial_df_wide, family = "binomial")
# summary(model)
# #####
# model <- glm(perc_binomial ~ max_plumes_fert, data = binomial_df_wide, family = "binomial")
# summary(model)
# #####
# model <- glm(perc_binomial ~ max_plumes_pest, data = binomial_df_wide, family = "binomial")
# summary(model)
# #####
# model <- glm(perc_binomial ~ max_population, data = binomial_df_wide, family = "binomial")
# summary(model)
# #####
# model <- glm(perc_binomial ~ max_shipping, data = binomial_df_wide, family = "binomial")
# summary(model)
# #####
# model <- glm(perc_binomial ~ max_Built2009, data = binomial_df_wide, family = "binomial")
# summary(model)
# #####
# model <- glm(perc_binomial ~ max_croplands2005, data = binomial_df_wide, family = "binomial")
# summary(model)
# #####
# model <- glm(perc_binomial ~ max_NavWater2009, data = binomial_df_wide, family = "binomial")
# summary(model)
# #####
# model <- glm(perc_binomial ~ max_Lights2009, data = binomial_df_wide, family = "binomial")
# summary(model)
# #####
# model <- glm(perc_binomial ~ max_Pasture2009, data = binomial_df_wide, family = "binomial")
# summary(model)
# #####
# model <- glm(perc_binomial ~ max_Popdensity2010, data = binomial_df_wide, family = "binomial")
# summary(model)
# #####
# model <- glm(perc_binomial ~ max_Roads, data = binomial_df_wide, family = "binomial")
# summary(model)
# #####
# model <- glm(perc_binomial ~ max_Railways, data = binomial_df_wide, family = "binomial")
# summary(model)
# 
# 
# 
# # Visualizing data before looking at GLMs of mean, max, min, sd
# plot(perc_binomial ~ mean, data = final_df_filtered)
# plot(percent ~ mean, data = final_df_filtered)
# boxplot(mean ~ percent , data= final_df_filtered)
# 
# # mean, max, sd, min models (without species or categories)
# ######
# # Let's try a model with means from all stressors & bird ranges
# # Does not converge when using "sp" as random factor
# model <- glm(perc_binomial ~ mean + (1|sp), data = final_df_filtered, family = "binomial")
# 
# # Plot of residuals
# resid.model = resid(model)
# plot(fitted(model), resid.model)
# abline(0,0)
# # Summary of model
# plot(model)
# summary(model)
# 
# # Obtaining odds of the slope (odds ratio) for explanatory variables 
# # (odds of probability of success for each explanatory variable)
# 
# exp(model$coefficients[2])
# # mean 
# # 0.2970207 
# # When the odds value is smaller than 1, interpretation is a little bit more complicated. 
# # When this is the case, we have to take the inverse value (i.e. 1 divided by the odds) to facilitate 
# # interpretation. The interpretation is then how LESS likely it is to observe the event of interest.
# 
# 
# # Confidence interval on the odds scale
# exp(confint(model)[2,])
# # 2.5 %    97.5 % 
# # 0.1109478 0.7845887 
# 
# # Goodness-of-fit
# # PseudoR2
# objects(model)
# pseudoR2 <- (model$null.deviance - model$deviance) / model$null.deviance
# pseudoR2
# # 0.007862609 very low R2
# 
# # Hosmer-Lemeshow test... evaluates whether the logistic regression model is a good fit for the data
# glmtoolbox::hltest(model)
# # p-value =  2.6391e-05 very low p-value, indicative of a poor fit
# 
# # Plotting model
# ggplot(final_df_filtered, aes(x = mean, y = perc_binomial)) + geom_point() + 
#   stat_smooth(method = "glm", family= "binomial", se = FALSE) + xlab("Mean exposure") +
#   ylab("Populastion trends") + 
#   ggtitle("Binomial population trends as a function of mean exposure")
# 
# ######
# 
# # Let's try a model with max values now 
# # Does not converge when using "sp" as random factor
# model <- glm(perc_binomial ~ max, data = final_df_filtered, family = "binomial")
# 
# # Plot of residuals
# resid.model = resid(model)
# plot(fitted(model), resid.model)
# abline(0,0)
# # Summary of model
# plot(model)
# summary(model)
# 
# # Obtaining odds of the slope (odds ratio) for explanatory variables (in this case, just mean exposure)
# exp(model$coefficients[2])
# # max 
# # 0.2134073
# # Confidence interval on the odds scale
# 
# exp(confint(model)[2,])
# # 2.5 %    97.5 % 
# # 0.1330352 0.3384291  
# 
# # Goodness-of-fit
# # PseudoR2
# objects(model)
# pseudoR2 <- (model$null.deviance - model$deviance) / model$null.deviance
# pseudoR2
# # 0.05865134 very low R2
# 
# # Hosmer-Lemeshow test... evaluates whether the logistic regression model is a good fit for the data
# glmtoolbox::hltest(model)
# # p-value =  0.43637, much higher than means
# 
# ######
# # standard deviation
# model <- glm(perc_binomial ~ sd, data = final_df_filtered, family = "binomial")
# 
# # Plot of residuals
# resid.model = resid(model)
# plot(fitted(model), resid.model)
# abline(0,0)
# # Summary of model
# plot(model)
# summary(model)
# 
# # Obtaining odds of the slope (odds ratio) for explanatory variables
# # (odds of probability of success for each explanatory variable)
# 
# exp(model$coefficients[2])
# # sd 
# # 0.002636511 
# # Confidence interval on the odds scale
# 
# exp(confint(model)[2,])
# # 2.5 %    97.5 % 
# # 0.0002972836 0.0212812842   
# 
# # Goodness-of-fit
# # PseudoR2
# objects(model)
# pseudoR2 <- (model$null.deviance - model$deviance) / model$null.deviance
# pseudoR2
# # 0.04316169 very low R2
# 
# # Hosmer-Lemeshow test... evaluates whether the logistic regression model is a good fit for the data
# glmtoolbox::hltest(model)
# # p-value = 0.0034068
# 
# ######
# model <- glm(perc_binomial ~ min, data = final_df_filtered, family = "binomial")
# 
# # Plot of residuals
# resid.model = resid(model)
# plot(fitted(model), resid.model)
# abline(0,0)
# # Summary of model
# plot(model)
# summary(model)
# 
# # Obtaining odds of the slope (odds ratio) for explanatory variables (in this case, just mean exposure)
# # (odds of probability of success for each explanatory variable)
# 
# exp(model$coefficients[2])
# # min 
# # 0.984218 
# 
# # Confidence interval on the odds scale
# exp(confint(model)[2,])
# # 2.5 %    97.5 % 
# # 0.3244942 2.9814796 
# 
# # Goodness-of-fit
# # PseudoR2
# objects(model)
# pseudoR2 <- (model$null.deviance - model$deviance) / model$null.deviance
# pseudoR2
# # 1.035565e-06 very low R2
# 
# # Hosmer-Lemeshow test... evaluates whether the logistic regression model is a good fit for the data
# glmtoolbox::hltest(model)
# # p-value = 0.75784 high p-value, better fit.. likely because of lots of 0's
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ##############################
# # Let's try a model with just one species, with multiple ranges; Long tailed Duck
# a <- "Long-tailed_Duck"
# lotduc <- subset(final_df_filtered, sp == a)
# 
# model_lotduc <- glm(perc_binomial ~ mean, data = lotduc, family = "binomial")
# summary(model_lotduc)
# 
# resid.lotduc <- resid(model_lotduc)
# 
# plot(fitted(model_lotduc), resid.lotduc)
# abline(0,0)
# 
# 
# 
# # # "Reworking" of population trends, adding a constant value so all values are above 0
# # # and a Poisson could be possible
# # new_perc <- final_df_filtered[, 6] + 95
# # new_perc <- as.data.frame(new_perc)
# # final_df_filtered <- cbind(final_df_filtered, new_perc)
# # 
# # # ANOVA (?)
# # # Estimates how a quantitative dependent variable changes 
# # # according to the levels of one or more categorical independent variables.
# # # In this case, how are population trends of certain species changing based on 
# # # varying levels of exposure to drivers?
# # 
# # model_a <- aov(new_perc ~ mean * sp, data = final_df_filtered)
# # plot(model_a)
# # summary(model_a)
# 
# 
# 
# 
# 

#### GLMER explorations

# Remove species with 0 population trends (for binomial approach)
binomial_df <- subset(final_df_filtered, subset = percent !='0')
# With wide dataset too
binomial_df_wide <- tidyr::pivot_wider(binomial_df, names_from = stressor, values_from = c(mean, max, min, sd))
# Now only 10 species, 22 ranges total(26 with amended NA removal, 12 species)

# Remove certain ranges to "look" for significance??? 

# didnt work
# # adjusting optimization parameters to potentially improve convergence. For example, you can increase the 
# # maximum number of iterations (nAGQ) and/or use a different optimization algorithm (e.g., bobyqa).
# model <- lme4::glmer(perc_binomial ~ mean_inorganic  + (1|sp), data = binomial_df_wide, family = "binomial", weights = time, nAGQ = 10, control = lme4::glmerControl(optimizer = "bobyqa"))
# summary(model)

# rescaling
# scaled_data <- scale(binomial_df_wide[, c("mean_inorganic", "range_scale", "lat", "long","time")])
# scaled_data <- cbind(scaled_data, binomial_df_wide[, c("perc_binomial", "sp")])
# 
# model <- lme4::glmer(perc_binomial ~ mean_inorganic + range_size + (1|sp), data = scaled_data, family = "binomial")
# summary(model)

# stressor specific models
#####

model <- lme4::glmer(perc_binomial ~  max_inorganic  + range_scale + (1|sp), data = binomial_df_wide, family = "binomial", weights = time_scale )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~  max_invasives  + range_scale  + (1|sp) , data = binomial_df_wide, family = "binomial" ,  weights = time_scale  )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~  max_lights + range_scale+ (1|sp)   , data = binomial_df_wide, family = "binomial",  weights = time_scale  )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~  max_ocean_pollution + range_scale+ (1|sp)   , data = binomial_df_wide, family = "binomial" ,  weights = time_scale )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~  max_plumes_fert + range_scale+ (1|sp)   , data = binomial_df_wide, family = "binomial" ,  weights = time_scale )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~  max_plumes_pest + range_scale+ (1|sp)   , data = binomial_df_wide, family = "binomial" ,  weights = time_scale )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~  max_population  + range_scale + (1|sp), data = binomial_df_wide, family = "binomial" ,  weights = time_scale)
summary(model)
#####
model <- lme4::glmer(perc_binomial ~  max_shipping + range_scale + (1|sp), data = binomial_df_wide, family = "binomial" ,  weights = time_scale )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~  max_Built2009 + range_scale + (1|sp)   , data = binomial_df_wide, family = "binomial",  weights = time_scale  )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~  max_croplands2005 + range_scale + (1|sp)   , data = binomial_df_wide, family = "binomial",  weights = time_scale  )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~  max_NavWater2009 + range_scale + (1|sp)   , data = binomial_df_wide, family = "binomial",  weights = time_scale  )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~  max_Lights2009 + range_scale + (1|sp)   , data = binomial_df_wide, family = "binomial",  weights = time_scale  )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~  max_Pasture2009 + range_scale + (1|sp)   , data = binomial_df_wide, family = "binomial",  weights = time_scale  )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~  max_Popdensity2010 + range_scale + (1|sp), data = binomial_df_wide, family = "binomial",  weights = time_scale  )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~  max_Roads + range_scale + (1|sp)   , data = binomial_df_wide, family = "binomial",  weights = time_scale  )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~  max_Railways + range_scale + (1|sp)   , data = binomial_df_wide, family = "binomial",  weights = time_scale  )
summary(model)

pp <- sjPlot::plot_model(model, type ="resid",
                         terms = c("max_Popdensity2010", "range_scale"), pred.type="re", axis.title = c("Light pollution","Probability"), legend.title = "Range size (scaled)", 
                         title= "Predicted Probabilities of Positive Population Trends", show.legend = FALSE)
pp

scatter.smooth(binomial_df_wide$max_Lights2009, binomial_df_wide$perc_binomial)
#####

# GLM's with removed 0 trend species and means, maxs, maxes, and min's for all stressors
# mean
model <- lme4::glmer(perc_binomial ~ mean + range_scale+ (1|sp), data = binomial_df, family = "binomial" )
summary(model)
# Plot of residuals vs. fitted
plot(model)

# max
model <- lme4::glmer(perc_binomial ~ max + range_scale + (1|sp), data = binomial_df, family = "binomial" )
summary(model)
# Plot of residuals vs. fitted
plot(model)

# min
model <- lme4::glmer(perc_binomial ~ min + (1|sp), data = binomial_df, family = "binomial")
summary(model)
# Plot of residuals vs. fitted
plot(model)

# sd
model <- lme4::glmer(perc_binomial ~ sd + (1|sp), data = binomial_df, family = "binomial" )
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

#########################
model <- lme4::glmer(perc_binomial ~ long + (1|sp), data = binomial_df_wide, family = "binomial")
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ range_scale + (1|sp), data = binomial_df_wide, family = "binomial" )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ range_scale + (1|sp), data = binomial_df_wide, family = "binomial" )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ range_scale + (1|sp), data = binomial_df_wide, family = "binomial" )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ range_scale + (1|sp), data = binomial_df_wide, family = "binomial" )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ range_scale + (1|sp), data = binomial_df_wide, family = "binomial" )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ range_scale + (1|sp), data = binomial_df_wide, family = "binomial" )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ range_scale + (1|sp), data = binomial_df_wide, family = "binomial" )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ range_scale + (1|sp), data = binomial_df_wide, family = "binomial" )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ range_scale + (1|sp), data = binomial_df_wide, family = "binomial" )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ range_scale + (1|sp), data = binomial_df_wide, family = "binomial" )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ range_scale + (1|sp), data = binomial_df_wide, family = "binomial" )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ range_scale + (1|sp), data = binomial_df_wide, family = "binomial" )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ range_scale + (1|sp), data = binomial_df_wide, family = "binomial" )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ range_scale + (1|sp), data = binomial_df_wide, family = "binomial" )
summary(model)
#####
model <- lme4::glmer(perc_binomial ~ range_scale + (1|sp), data = binomial_df_wide, family = "binomial" )
summary(model)



write.csv(binomial_df, file = "test.csv", row.names = FALSE)

binomial_1 <- read.csv("binomial_1.csv")
binomial_0 <- read.csv("binomial_0.csv")

binomial_1_wide <- tidyr::pivot_wider(binomial_1, names_from = stressor, values_from = c(mean, max, min, sd))

hist(binomial_df$mean)
hist(binomial_0$mean)
hist(binomial_1$mean)

model <- lme4::glmer(perc_binomial ~ mean + (1|sp), data = binomial_1_wide, family = "binomial")


