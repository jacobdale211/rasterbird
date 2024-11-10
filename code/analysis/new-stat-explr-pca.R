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




#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# PCA -- Associations of environmental drivers
library(tidyverse)
library(FactoMineR)
library(factoextra)

data <- read.csv("res_terra_edit_moresp.csv")

# List of environmental variables
variables <- c("inorganic", "invasives", "lights_halp", "ocn_pol", "plm_fert",
               "plm_pest", "pop_halp", "built", "croplands", "lights_vent",
               "navwater", "pastures", "pop_vent", "railways", "roads")


# Aggregate data by species
aggregated_data <- data %>%
  group_by(sp) %>%
  summarize(across(all_of(variables), mean, na.rm = TRUE),
            perc = first(perc))

# Perform PCA
pca_result <- PCA(aggregated_data[, variables], graph = FALSE)

# Scree plot
fviz_eig(pca_result, addlabels = TRUE)

# Variables plot
fviz_pca_var(pca_result, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# Individuals plot (species)
fviz_pca_ind(pca_result, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

# Biplot
fviz_pca_biplot(pca_result, repel = TRUE,
                col.var = "#2E9FDF", 
                col.ind = "#696969"  
)

# Print summary of PCA
print(summary(pca_result))

# Correlation of variables with principal components
print(pca_result$var$cor)

# Contribution of variables to principal components
print(pca_result$var$contrib)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# PCA with species points colors



library(tidyverse)
library(FactoMineR)
library(factoextra)

# Read the data
data <- read.csv("res_terra_edit_moresp.csv")

# List of environmental variables
variables <- c("inorganic", "invasives", "lights_halp", "ocn_pol", "plm_fert",
               "plm_pest", "pop_halp", "built", "croplands", "lights_vent",
               "navwater", "pastures", "pop_vent", "railways", "roads")

# Create a vector identifying bird types
bird_types <- c(
  "American_Golden-Plover" = "shorebird",
  "Bairds_Sandpiper" = "shorebird",
  "Black-bellied_Plover" = "shorebird",
  "Buff-breasted_Sandpiper" = "shorebird",
  "Cackling_Goose" = "waterfowl",
  "King_Eider" = "waterfowl",
  "Long-tailed_Duck" = "waterfowl",
  "Pectoral_Sandpiper" = "shorebird",
  "Red-throated_Loon" = "waterfowl",
  "Red_Knot" = "shorebird",
  "Ruddy_Turnstone" = "shorebird",
  "Snow_Goose" = "waterfowl",
  "Tundra_Swan" = "waterfowl",
  "White-rumped_Sandpiper" = "shorebird"
)

# Aggregate data by species
aggregated_data <- data %>%
  group_by(sp) %>%
  summarize(across(all_of(variables), mean, na.rm = TRUE),
            perc = first(perc))

# Add bird type to aggregated data
aggregated_data$bird_type <- bird_types[aggregated_data$sp]

# Perform PCA
pca_result <- PCA(aggregated_data[, variables], graph = FALSE)

# Create biplot with custom colors
fviz_pca_biplot(pca_result, repel = TRUE,
                col.var = "darkgrey", # Color for variable arrows
                col.ind = as.factor(aggregated_data$bird_type), # Color points by bird type
                palette = c("shorebird" = "#72bcd4", "waterfowl" = "#4ee44f"),
                mean.point = FALSE,
                legend.title = "Bird Type"
)




#---------------------------------------------------------------------
# Last minute LM !! (LM LM)

library(tidyverse)
library(ggplot2)
library(ggrepel)

data <- read.csv("res_terra_edit_moresp.csv")

shorebirds <- c("American_Golden-Plover", "Bairds_Sandpiper", 
                "Black-bellied_Plover", "Buff-breasted_Sandpiper",
                "Pectoral_Sandpiper", "Red_Knot", 
                "Ruddy_Turnstone", "White-rumped_Sandpiper", "Long-tailed_Duck")

variables <- c("inorganic", "invasives", "lights_halp", "ocn_pol", "plm_fert",
               "plm_pest", "pop_halp", "built", "croplands", "lights_vent",
               "navwater", "pastures", "pop_vent", "railways", "roads")

shorebird_data <- data %>%
  filter(sp %in% shorebirds) %>%
  group_by(sp) %>%
  summarize(
    mce = mean(rowSums(across(all_of(variables))), na.rm = TRUE),
    trend = first(perc)
  )

model <- lm(trend ~ mce, data = shorebird_data)

summary(model)

#plot w regression line
ggplot(shorebird_data, aes(x = mce, y = trend)) +
  geom_point(color = "#72bcd4", size = 3) +
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = sp), color = "black") +
  theme_minimal() +
  labs(x = "Mean Cumulative Exposure",
       y = "Population Trend (%)",
       title = "Shorebird Population Trends vs. Mean Cumulative Exposure") +
  theme(plot.title = element_text(hjust = 0.5))

# Print the data
print(shorebird_data)

spearman <- cor.test(shorebird_data$mce, shorebird_data$trend)

rho <- spearman$estimate
rho
rho_pvalue <- spearman$p.value
rho_pvalue


#-----------------------------------------------
# ! lets do it with waterfowl

library(tidyverse)
library(ggrepel)

data <- read.csv("res_terra_edit_moresp.csv")


waterfowl <- c("Cackling_Goose", "Long-tailed_Duck",
               "Red-throated_Loon", "Snow_Goose", "Tundra_Swan")

variables <- c("inorganic", "invasives", "lights_halp", "ocn_pol", "plm_fert",
               "plm_pest", "pop_halp", "built", "croplands", "lights_vent",
               "navwater", "pastures", "pop_vent", "railways", "roads")

waterfowl_data <- data %>%
  filter(sp %in% waterfowl) %>%
  group_by(sp) %>%
  summarize(
    mce = mean(rowSums(across(all_of(variables))), na.rm = TRUE),
    trend = first(perc)
  )

model <- lm(trend ~ mce, data = waterfowl_data)

summary(model)

ggplot(waterfowl_data, aes(x = mce, y = trend)) +
  geom_point(color = "#4ee44f", size = 3) +  # Changed color to green for waterfowl
  geom_smooth(method = "lm", color = "red") +
  geom_text_repel(aes(label = sp), color = "black") +
  theme_minimal() +
  labs(x = "Mean Cumulative Exposure",
       y = "Population Trend (%)",
       title = "Waterfowl Population Trends vs. Mean Cumulative Exposure") +
  theme(plot.title = element_text(hjust = 0.5))











#--------------------------------------------------------
#wilcoxon
library(dplyr)

library(tidyverse)

# Read the data
data <- read.csv("res_terra_edit_moresp.csv")

variables <- c("inorganic", "invasives", "lights_halp", "ocn_pol", "plm_fert",
               "plm_pest", "pop_halp", "built", "croplands", "lights_vent",
               "navwater", "pastures", "pop_vent", "railways", "roads")
# remove king eider "King_Eider",
waterfowl <- c("Cackling_Goose",   
               "Red-throated_Loon", "Snow_Goose", "Tundra_Swan")

shorebirds <- c("American_Golden-Plover", "Bairds_Sandpiper", 
                "Black-bellied_Plover", "Buff-breasted_Sandpiper",
                "Pectoral_Sandpiper", "Red_Knot", 
                "Ruddy_Turnstone", "White-rumped_Sandpiper", "Long-tailed_Duck", "King_Eider")

species_exposure <- data %>%
  dplyr::filter(sp %in% c(waterfowl, shorebirds)) %>%
  group_by(sp) %>%
  summarize(
    mce = mean(rowSums(across(all_of(variables))), na.rm = TRUE)
  ) %>%
  mutate(group = ifelse(sp %in% waterfowl, "Waterfowl", "Shorebird"))

wilcox.test(mce ~ group, data = species_exposure)

ggplot(species_exposure, aes(x = group, y = mce, fill = group)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  scale_fill_manual(values = c("Shorebird" = "#72bcd4", "Waterfowl" = "#4ee44f")) +
  theme_minimal() +
  labs(x = "Species Group",
       y = "Mean Cumulative Exposure",
       title = "Comparison of Cumulative Exposure Between Groups")

#----------------------------------------------------
# wilcoxon for dhp and urban expansion only 
library(tidyverse)

# Define bird types
waterfowl <- c("Cackling_Goose", "Red-throated_Loon", 
               "Snow_Goose", "Tundra_Swan", "Long-tailed_Duck")

shorebirds <- c("American_Golden-Plover", "Bairds_Sandpiper", 
                "Black-bellied_Plover", "Buff-breasted_Sandpiper",
                "Pectoral_Sandpiper", "Red_Knot", 
                "Ruddy_Turnstone", "White-rumped_Sandpiper"
                 )

# Read the data
data <- read.csv("csvs/sumsts_tr.csv")

# Filter for Direct Human Presence and Urban Expansion categories
# and calculate mean by species, adding type based on the vectors
exposure_by_species <- data %>%
  filter(category %in% c("Direct Human Presence", "Urban Expansion")) %>%
  group_by(sp) %>%
  summarize(mean_exposure = mean(mean, na.rm = TRUE)) %>%
  mutate(type = case_when(
    sp %in% waterfowl ~ "waterfowl",
    sp %in% shorebirds ~ "shorebird",
    TRUE ~ NA_character_
  ))

expos <- exposure_by_species

expos <- exposure_by_species %>%
  filter(!grepl("King_Eider", sp))
  
# Perform Wilcoxon test
wilcox_result <- wilcox.test(mean_exposure ~ type, data = expos)

# Create boxplot to visualize
ggplot(expos, aes(x = type, y = mean_exposure, fill = type)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  scale_fill_manual(values = c("shorebird" = "#72bcd4", "waterfowl" = "#4ee44f")) +
  theme_minimal() +
  labs(x = "Species Group",
       y = "Mean Cumulative Exposure",
       title = "Comparison of Cumulative Exposure Between Groups\n(Direct Human Presence & Urban Expansion)")

# Print test results
print(wilcox_result)

# Print summary statistics
print(expos %>%
        group_by(type) %>%
        summarize(
          mean = mean(mean_exposure),
          sd = sd(mean_exposure),
          n = n()
        ))

#--------------------------------------------------------------
# wilcoxon for negative v positive trends
#wilcoxon
library(dplyr)

library(tidyverse)

# Read the data
data <- read.csv("res_terra_edit_moresp.csv")

variables <- c("inorganic", "invasives", "lights_halp", "ocn_pol", "plm_fert",
               "plm_pest", "pop_halp", "built", "croplands", "lights_vent",
               "navwater", "pastures", "pop_vent", "railways", "roads")
# remove king eider "King_Eider",
waterfowl <- c("Cackling_Goose",   
               "Red-throated_Loon", "Snow_Goose", "Tundra_Swan")

shorebirds <- c("American_Golden-Plover", "Bairds_Sandpiper", 
                "Black-bellied_Plover", "Buff-breasted_Sandpiper",
                "Pectoral_Sandpiper", "Red_Knot", 
                "Ruddy_Turnstone", "White-rumped_Sandpiper", "Long-tailed_Duck")

species_exposure <- data %>%
  dplyr::filter(sp %in% c(waterfowl, shorebirds)) %>%
  group_by(sp) %>%
  summarize(
    mce = mean(rowSums(across(all_of(variables))), na.rm = TRUE)
  ) %>%
  mutate(group = ifelse(sp %in% waterfowl, "Positive trends", "Negative trends"))

wilcox.test(mce ~ group, data = species_exposure)

ggplot(species_exposure, aes(x = group, y = mce, fill = group)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  scale_fill_manual(values = c("Negative trends" = "red", "Positive trends" = "purple")) +
  theme_minimal() +
  labs(x = "Species Group",
       y = "Mean Cumulative Exposure",
       title = "Comparison of Cumulative Exposure Between Groups")





# a simple test to maybe stress me out
library(tidyverse)

sumsts_tr <- read.csv("csvs/sumsts_tr.csv")
# Define shorebirds
shorebirds <- c("American_Golden-Plover", "Bairds_Sandpiper", 
                "Black-bellied_Plover", "Buff-breasted_Sandpiper",
                "Pectoral_Sandpiper", "Red_Knot", 
                "Ruddy_Turnstone", "White-rumped_Sandpiper")

# For Croplands model
croplands_data <- sumsts_tr %>%
  filter(sp %in% shorebirds,
         driver == "Croplands") %>%
  select(sp, mean, perc)

# For Agricultural drivers model
ag_data <- sumsts_tr %>%
  filter(sp %in% shorebirds,
         category == "Agriculture") %>%
  group_by(sp) %>%
  summarize(mean_ag = mean(mean),
            perc = first(perc))

# Create linear models
model_croplands <- lm(perc ~ mean, data = croplands_data)
model_agriculture <- lm(perc ~ mean_ag, data = ag_data)

# Print summaries
print("Croplands Model (Population Change ~ Croplands):")
summary(model_croplands)

print("Agricultural Drivers Model (Population Change ~ Mean Agricultural Exposure):")
summary(model_agriculture)

# Visualizations
# Croplands
ggplot(croplands_data, aes(x = mean, y = perc)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(x = "Mean Exposure to Croplands",
       y = "Population Change (%)",
       title = "Shorebird Population Change vs. Croplands Exposure")

# Agriculture
ggplot(ag_data, aes(x = mean_ag, y = perc)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(x = "Mean Exposure to Agricultural Drivers",
       y = "Population Change (%)",
       title = "Shorebird Population Change vs. Agricultural Exposure")

