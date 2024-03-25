###


res <- read.csv("res_terra_edit_moresp.csv")
# Remove extra columns that pop up after exporting too many times
library(dplyr)
library(tidyr)
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
result <- calculate_stats(res, value_column = inorganic, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Inorganic pollution")
performance::r2(model)

### invasives
result <- calculate_stats(res, value_column = invasives, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Invasive species presence")
performance::r2(model)

### lights, halpern
result <- calculate_stats(res, value_column = lights_halp, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Light pollution (Halpern)")
performance::r2(model)

### ocean pollution
result <- calculate_stats(res, value_column = ocn_pol, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Ocean Pollution")
performance::r2(model)

### fertilizer plumes
result <- calculate_stats(res, value_column = plm_fert, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Fertilizer plumes")
performance::r2(model)

### pesticide plumes
result <- calculate_stats(res, value_column = plm_pest, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Pesticide Plumes")
performance::r2(model)

### population, halpern
result <- calculate_stats(res, value_column = pop_halp, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Population density (Halpern)")
performance::r2(model)

### shipping
result <- calculate_stats(res, value_column = ship, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Shipping")
performance::r2(model)


###venter


### urban environments
result <- calculate_stats(res, value_column = built, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Urban Environments")
performance::r2(model)

### croplands
result <- calculate_stats(res, value_column = croplands, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Cropland distribution")
performance::r2(model)

### lights, venter
result <- calculate_stats(res, value_column = lights_vent, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Light pollution (Venter)")
performance::r2(model)

### waterways
result <- calculate_stats(res, value_column = navwater, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Waterway distribution")
performance::r2(model)

### pastures
result <- calculate_stats(res, value_column = pastures, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Pasture distribution")
performance::r2(model)

### population, venter
result <- calculate_stats(res, value_column = pop_vent, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Population density (Venter)")
performance::r2(model)

### railways
result <- calculate_stats(res, value_column = railways, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Railway distribution")
performance::r2(model)

### roads
result <- calculate_stats(res, value_column = roads, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean  + range_scale + (1|sp), data = result, family = "binomial", weights = time)
summary(model)
hist(result$mean)
plot(result$mean, result$perc_binomial, main = "Road distribution")
performance::r2(model)

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

### invasives
result <- calculate_stats(res, value_column = invasives, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean+ (1|sp), data = result, family = "binomial" )
summary(model)

### lights, halpern
result <- calculate_stats(res, value_column = lights_halp, 
                          group_column1 = sp, group_column2 = range_scale, group_column3 = range, 
                          group_column4 = perc_binomial, group_column5 = time)
model <- lme4::glmer(perc_binomial ~  mean   + (1|sp), data = result, family = "binomial" )
summary(model)

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
model <- lme4::glmer(perc_binomial ~  mean   + (1|sp), data = result, family = "binomial" )
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
model <- lme4::glmer(perc_binomial ~  mean   + (1|sp), data = result, family = "binomial" )
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
