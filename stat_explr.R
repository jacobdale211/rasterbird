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
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
scatter.smooth(result$mean, result$perc_binomial, main = "Inorganic pollution")
performance::r2(model)
visreg(model)


### invasives
result_invasives <- calculate_stats(res, value_column = invasives, 
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

