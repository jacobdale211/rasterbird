res <- read.csv("res_terra_edit_moresp.csv")

# Remove extra columns that pop up after exporting too many times
library(dplyr)
library(tidyr)
library(visreg)
s_t_r <- c("x")
res <- res %>%
  select(-contains(s_t_r))


test <- res %>%
  select("sp", "croplands")
# tables for data for each driver
# run inidividually for each driver
values <- function(data, species, value_column) {
  stats <- data %>%
    group_by(sp) %>%
    summarise(mean = mean({{value_column}}, na.rm = TRUE),
              min = min({{value_column}}, na.rm = TRUE),
              max = max({{value_column}}, na.rm = TRUE),
              sd = sd({{value_column}}, na.rm = TRUE))
  
}



results <- values(res, species = sp, value_column = inorganic)
write.csv(results, "inorganic16.csv")

results <- values(res, species = sp, value_column = invasives)
write.csv(results, "invasives16.csv")

results <- values(res, species = sp, value_column = lights_halp)
write.csv(results, "light_halp16.csv")

results <- values(res, species = sp, value_column = ocn_pol)
write.csv(results, "ocnpol16.csv")

results <- values(res, species = sp, value_column = plm_fert)
write.csv(results, "plmfert16.csv")

results <- values(res, species = sp, value_column = plm_pest)
write.csv(results, "plmpest16.csv")

results <- values(res, species = sp, value_column = pop_halp)
write.csv(results, "pophalp16.csv")

results <- values(res, species = sp, value_column = ship)
write.csv(results, "ship16.csv")

results <- values(res, species = sp, value_column = built)
write.csv(results, "built16.csv")

results <- values(res, species = sp, value_column = croplands)
write.csv(results, "croplands16.csv")

results <- values(res, species = sp, value_column = lights_vent)
write.csv(results, "lightvent16.csv")

results <- values(res, species = sp, value_column = navwater)
write.csv(results, "navwater16.csv")

results <- values(res, species = sp, value_column = pastures)
write.csv(results, "pastures16.csv")

results <- values(res, species = sp, value_column = pop_vent)
write.csv(results, "popvent16.csv")

results <- values(res, species = sp, value_column = railways)
write.csv(results, "railways16.csv")

results <- values(res, species = sp, value_column = roads)
write.csv(results, "roads16.csv")


