sumsts <- read.csv("sumsts.csv")
# 
# sumsts <- sumsts %>%
#   mutate(perc = case_when(
#     sp == "American_Golden-Plover" ~ "-73.31",
#     sp == "Bairds_Sandpiper" ~ "-5.28",
#     sp == "Black-bellied_Plover" ~ "-57.39",
#     sp == "Buff-breasted_Sandpiper" ~ "-58.17",
#     sp == "Cackling_Goose" ~ "833.13",
#     sp == "Glaucous_Gull" ~ "0",
#     sp == "King_Eider" ~ "-83.34",
#     sp == "Long-tailed_Duck" ~ "-65.04",
#     sp == "Long-tailed_Jaeger" ~ "0",
#     sp == "Pacific_Loon" ~ "0",
#     sp == "Parasitic_Jaeger" ~ "0",
#     sp == "Pectoral_Sandpiper" ~ "-64.40",
#     sp == "Red_Knot" ~ "-94.05",
#     sp == "Red-throated_Loon" ~ "21.18",
#     sp == "Ruddy_Turnstone" ~ "-76.29",
#     sp == "Snow_Goose" ~ "477.0",
#     sp == "Tundra_Swan" ~ "3.99",
#     sp == "White-rumped_Sandpiper" ~ "-26.57",
#   ))
sumsts$perc <- as.numeric(sumsts$perc)
sumsts <- sumsts %>%
  select(-max, -min, -sd)

amg <- sumsts[grepl("American_Golden-Plover", sumsts$sp), ]
amg <- amg[grepl("155", amg$X), ] # looking at just population density (venter)



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

# Initialize an empty dataframe to store results
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



# nixed model might work(?)
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
data <- read.csv("res_terra_edit_moresp.csv")

library(lme4)

mixed_model <- lmer(perc ~ (1|sp) + inorganic + invasives + lights_halp + ocn_pol + 
                      plm_fert + plm_pest + pop_halp + ship + built + croplands + 
                      lights_vent + navwater + pastures + pop_vent + railways + roads, 
                    data = data)
summary(mixed_model)



### RDA
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
library(vegan)
library(tidyverse)

# Read the data
data <- read.csv("res_terra_edit_moresp.csv")

# List of environmental stressors
stressors <- c("inorganic", "invasives", "lights_halp", "ocn_pol", "plm_fert", 
               "plm_pest", "pop_halp", "ship", "built", "croplands", "lights_vent", 
               "navwater", "pastures", "pop_vent", "railways", "roads")

# Aggregate data by species
aggregated_data <- data %>%
  group_by(sp) %>%
  summarize(across(all_of(stressors), mean, na.rm = TRUE),
            perc = first(perc))  # Assuming perc is constant for each species

# Separate response variables (species trends) and explanatory variables
species_trends <- aggregated_data %>% select(sp, perc) %>% column_to_rownames("sp")
environmental_data <- aggregated_data %>% select(all_of(stressors))

# Perform RDA
rda_result <- rda(species_trends ~ ., data = environmental_data)

# Summary of RDA results
summary(rda_result)

# Plot RDA results
plot(rda_result)

# Test significance of RDA result
anova(rda_result)

# Test significance of individual terms
anova(rda_result, by = "terms")

# Calculate and print the variance inflation factors
vif.cca(rda_result)

# Additional visualization: triplot
# This will create a more detailed plot showing species, sites, and environmental variables
triplot <- ordiplot(rda_result, type = "none")
points(triplot, "sites", pch = 21, col = "black", bg = "steelblue")
text(triplot, "species", col = "red", cex = 0.8)
arrows(0, 0, scores(rda_result, choices = 1:2, display = "bp")[,1],
       scores(rda_result, choices = 1:2, display = "bp")[,2],
       col = "dark