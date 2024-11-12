res <- read.csv("csvs/res_terra_edit_moresp.csv")

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
#select only amg
amg <- res %>%
  filter(grepl("American_Golden-Plover", sp))
#remove extraneous columns
amg <- amg %>%
  select(-range, -range_size, -y, -coverage_fraction, -perc, -perc_binomial, -time, -range_scale)



 results <- values(res, species = sp, value_column = navwater)
# 
# # summed dataframes for driver categories -- urban environments
nav <- values(res, species = sp, value_column = navwater)
nav$driver <- "Waterways"
write.csv(nav, "navwater16.csv")

ralwy <- values(res, species = sp, value_column = railways)
ralwy$driver <- "Railways"
write.csv(ralwy, "railways16.csv")

rds <- values(res, species = sp, value_column = roads)
rds$driver <- "Roads"
 write.csv(rds, "roads16.csv")

blt <- values(res, species = sp, value_column = built)
blt$driver <- "Urban Environments"
write.csv(blt, "built16.csv")

# nav <- subset(nav, select = -sp)
# ralwy <- subset(ralwy, select = -sp)
# rds <- subset(rds, select = -sp)
# blt <- subset(blt, select = -sp)
# q <- list(nav, ralwy, rds, blt)
# 
# urbev <- Reduce(`+`, q)
# urbev$sp <- results$sp
# urbev$category <- "Urban Expansion"
# write.csv(urbev, "urbev.csv")

# par(mfrow = c(1, 1))
# barplot(sd ~ sp, data = summed_df)

# marine pollution
ino <- values(res, species = sp, value_column = inorganic)
ino$driver <- "Inorganic pollution"
  write.csv(ino, "inorganic16.csv")

inva <- values(res, species = sp, value_column = invasives)
inva$driver <- "Invasive species"
 write.csv(inva, "invasives16.csv")

ocn <- values(res, species = sp, value_column = ocn_pol)
ocn$driver <- "Ocean pollution"
write.csv(ocn, "ocnpol16.csv")
# 
# shpg <- values(res, species = sp, value_column = ship)
# shpg$driver <- "Shipping"
# write.csv(shpg, "ship16.csv")

# ino <- subset(ino, select = -sp)
# inva <- subset(inva, select = -sp)
# ocn <- subset(ocn, select = -sp)
# shpg <- subset(shpg, select = -sp)
# q2 <- list(ino, inva, ocn, shpg)
# 
# marpol <- Reduce(`+`, q2)
# marpol$sp <- results$sp
# marpol$category <- "Marine Pollution"
# write.csv(marpol, "marpol.csv")

# direct human presence
lhal <- values(res, species = sp, value_column = lights_halp)
lhal$driver <- "Light pollution (Halpern)"
write.csv(lhal, "light_halp16.csv")

lvent <- values(res, species = sp, value_column = lights_vent)
lvent$driver <- "Light pollution (Venter)"
write.csv(lvent, "lightvent16.csv")

pvent <- values(res, species = sp, value_column = pop_vent)
pvent$driver <- "Population density (Venter)"
write.csv(pvent, "popvent16.csv")

phal <- values(res, species = sp, value_column = pop_halp)
phal$driver <- "Population density (Halpern)"
write.csv(phal, "pophalp16.csv")

# lhal <- subset(lhal, select = -sp)
# lvent <- subset(lvent, select = -sp)
# pvent <- subset(pvent, select = -sp)
# phal <- subset(phal, select = -sp)
# q3 <- list(lhal, lvent, phal, pvent)
# 
# dhp <- Reduce(`+`, q3)
# dhp$sp <- results$sp
# dhp$category <- "Direct Human Presence"
# write.csv(dhp, "dhp.csv")


# agriculture
fert <- values(res, species = sp, value_column = plm_fert)
fert$driver <- "Fertilizer plumes"
write.csv(fert, "plmfert16.csv")

pest <- values(res, species = sp, value_column = plm_pest)
pest$driver <- "Pesticide plumes"
write.csv(pest, "plmpest16.csv")

pstr <- values(res, species = sp, value_column = pastures)
pstr$driver <- "Pastures"
write.csv(pstr, "pastures16.csv")

crop <- values(res, species = sp, value_column = croplands)
crop$driver <- "Croplands"
write.csv(crop, "croplands16.csv")
#
# fert <- subset(fert, select = -sp)
# pest <- subset(pest, select = -sp)
# pstr <- subset(pstr, select = -sp)
# crop <- subset(crop, select = -sp)
# q4 <- list(fert, pest, pstr, crop)
# 
# agr <- Reduce(`+`, q4)
# agr$sp <- results$sp
# agr$category <- "Agriculture"
# write.csv(agr, "agr.csv")

all_stats <- rbind(blt, nav, ralwy, rds,
                   ino, inva, ocn, 
                   lhal, lvent, phal, pvent,
                   fert, pest, pstr, crop)
# adding category based on driver
assign_category <- function(x) {
  ifelse(x %in% c("Urban Environments", "Waterways", "Railways", "Roads"), "Urban Expansion",
         ifelse(x %in% c("Inorganic pollution", "Invasive species", "Ocean pollution" ), "Marine pollution",
                ifelse(x %in% c("Fertilizer plumes", "Pesticide plumes", "Pastures", "Croplands"), "Agriculture",
                "Direct Human Presence")))
  }
all_stats$category <- assign_category(all_stats$driver)
write.csv(all_stats, "sumsts_noshpg.csv")

# creating species tables
sumsts <- read.csv("csvs/sumsts.csv")
spl <- split(sumsts, sumsts$sp)
for (i in seq_along(spl)) {
  species_name <- names(spl)[i]
  df <- spl[[i]]
  
  write.csv(df, file = paste0("df_species_", species_name, ".csv"), row.names = FALSE)
}

# # # # #
library(dplyr)
sumsts <- read.csv("csvs/sumsts.csv")


# sorting out tables to be species only
filter_dataframe <- function(df, column, value) {
  df %>% filter(!!sym(column) == value)
}

amgplo <- filter_dataframe(sumsts, "sp", "American_Golden-Plover")
# # # # # # # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# cumulative exposure table
library(dplyr)
df <- read_csv("csvs/sumsts_noshpg.csv")

# Clean up the species names by removing underscores
df <- df %>%
  mutate(sp = gsub("_", " ", sp))

# Group by species and summarize
result <- df %>%
  group_by(sp) %>%
  summarize(total_max = sum(max, na.rm = TRUE)) %>%
  arrange(desc(total_max))
# # # # # # # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# categories
# urbev <- read.csv("urbev.csv")
# marpol <- read.csv("marpol.csv")
# dhp <- read.csv("dhp.csv")
# agr <- read.csv("agr.csv")


# # # # # # ---------------------------------------------------------------
# Bar plot starts here
# # # # # # ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)



sumsts <- read.csv("csvs/sumsts_tr.csv")
bplot <- sumsts %>%
  select(-min, -max, -sd)
bplot <- bplot %>% 
  mutate(sp = gsub("_", " ", sp))
#param
xG = .3
yG = .02





di <- bplot %>%
  group_by(sp, driver, category) %>%
  summarise(mean_sum = sum(mean, na.rm = TRUE), .groups = "drop") %>%
  group_by(sp) %>%
  mutate(species_total = sum(mean_sum)) %>%
  ungroup() %>%
  arrange(desc(species_total), category, desc(mean_sum)) %>%
  mutate(
    sp = factor(sp, levels = unique(sp)),
    category = factor(category, levels = c("Urban Expansion", "Marine pollution", "Direct Human Presence", "Agriculture"))
  )

category_colors <- c("Urban Expansion" = "#FF9999", "Marine pollution" = "#66B2FF",
                     "Direct Human Presence" = "#99FF99", "Agriculture" = "#FFCC99")

# plot with proper stack alphas
p <- ggplot(di, aes(x = sp, y = mean_sum, fill = category, alpha = driver)) +
  geom_col(position = "stack", width = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  scale_fill_manual(values = category_colors) +
  scale_alpha_manual(values = seq(0.4, 1, length.out = n_distinct(di$driver))) +
  theme_minimal(base_family = "serif") +  # Set base font to serif (Times New Roman)
  theme(
    text = element_text(family = "serif"),  # Ensure all text uses serif font
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(family = "serif"),
    axis.title = element_text(family = "serif"),
    legend.title = element_text(family = "serif"),
    legend.text = element_text(family = "serif")
  ) +
  labs(x = NULL, y = "Impact Score", fill = "Category", alpha = "Driver")

# Display the plot
print(p)

# ===========================================================================================
library(ggplot2)
library(dplyr)

# Assuming 'di' dataframe is already prepared

# Define category colors
category_colors <- c(
  "Urban Expansion" = "#FF9999",
  "Marine pollution" = "#66B2FF",
  "Direct Human Presence" = "#99FF99",
  "Agriculture" = "#FFCC99"
)

# Create a named vector for driver alpha values
driver_alphas <- setNames(seq(0.3, 1, length.out = n_distinct(di$driver)), 
                          sort(unique(di$driver)))

# Ensure category is a factor with the correct order
di <- di %>%
  mutate(category = factor(category, levels = names(category_colors)))

# Calculate total impact for each species and join it back to the main dataframe
di <- di %>%
  group_by(sp) %>%
  mutate(species_total = sum(mean_sum, na.rm = TRUE)) %>%
  ungroup()

p <- ggplot(di, aes(x = reorder(sp, -species_total), y = mean_sum, fill = category, alpha = driver)) +
  geom_col(position = "stack", width = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  scale_fill_manual(values = category_colors) +
  scale_alpha_manual(values = driver_alphas, guide = guide_legend(ncol = 1)) +
  theme_minimal(base_family = "serif") +
  theme(
    text = element_text(family = "serif"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.key.size = unit(0.5, "cm"),
    legend.text = element_text(size = 6)
  ) +
  labs(x = NULL, y = "Impact Score", fill = "Category", alpha = "Driver")

# Display the plot
print(p)

# If you want to save the plot
# ggsave("category_color_driver_alpha_stacked_bar_chart.png", p, width = 14, height = 8, dpi = 300)




# - # - # ---------------------------------  trying the base plot function



# Load required libraries
library(dplyr)
sumsts <- read.csv("csvs/sumsts_noshpg.csv")
bplot <- sumsts %>%
  select(-min, -max, -sd)
bplot <- bplot %>% 
  mutate(sp = gsub("_", " ", sp))
#param
xG = .3
yG = .02
# Assuming 'di' dataframe is already prepared
di <- bplot %>%
  group_by(sp, driver, category) %>%
  summarise(mean_sum = sum(mean, na.rm = TRUE), .groups = "drop") %>%
  group_by(sp) %>%
  mutate(species_total = sum(mean_sum)) %>%
  ungroup() %>%
  arrange(desc(species_total), category, desc(mean_sum)) %>%
  mutate(
    sp = factor(sp, levels = unique(sp)),
    category = factor(category, levels = c("Urban Expansion", "Marine pollution", "Direct Human Presence", "Agriculture"))
  )




library(dplyr)
library(tidyr)

# Define category colors
category_colors <- c(
  "Urban Expansion" = "#FF9999",
  "Marine pollution" = "#66B2FF",
  "Direct Human Presence" = "#99FF99",
  "Agriculture" = "#FFCC99"
)

# Define fixed alpha values
fixed_alphas <- c(0.7, 0.8, 0.9, 1)

# Create a dataframe with driver information, ensuring consistent alpha assignment
driver_info_df <- di %>%
  group_by(category) %>%
  summarise(drivers = list(unique(driver))) %>%
  rowwise() %>%
  mutate(
    alphas = list(fixed_alphas[1:length(drivers)]),
    colors = list(rep(category_colors[category], length(drivers)))
  ) %>%
  unnest(cols = c(drivers, alphas, colors)) %>%
  rename(driver = drivers) %>%
  select(driver, category, color = colors, alpha = alphas) %>%
  arrange(category, alpha)  # Ensure drivers are ordered by alpha within category

# Create the driver_alphas vector needed for the plot
driver_alphas <- setNames(driver_info_df$alpha, driver_info_df$driver)

# Calculate total impact for each species, category, and driver
plot_data <- di %>%
  group_by(sp, category, driver) %>%
  summarise(total = sum(mean_sum, na.rm = TRUE), .groups = 'drop') %>%
  left_join(driver_info_df, by = c("category", "driver")) %>%
  arrange(sp, category, alpha)  # Order by alpha within each category

# Calculate cumulative totals for positioning, ensuring correct stacking order
plot_data <- plot_data %>%
  group_by(sp) %>%
  arrange(category, alpha) %>%  # This ensures correct stacking order
  mutate(
    cumulative_total = cumsum(total),
    cumulative_total_prev = lag(cumulative_total, default = 0)
  ) %>%
  ungroup()

# Set up the plot
png("base_r_stacked_bar_chart_ordered_alphas.png", width = 1200, height = 800, res = 100)
par(mar = c(10, 5, 4, 10), family = "serif")

# Create empty plot
plot(0, type = "n", xlim = c(0, length(unique(plot_data$sp))), ylim = c(0, max(plot_data$cumulative_total) * 1.1),
     xlab = "", ylab = "Impact Score", xaxt = "n", yaxt = "n")

# Add y-axis
axis(2, las = 1)

# Draw bars with fixed alpha for each driver, now in correct order
for (i in 1:nrow(plot_data)) {
  rect(which(unique(plot_data$sp) == plot_data$sp[i]) - 0.45, plot_data$cumulative_total_prev[i],
       which(unique(plot_data$sp) == plot_data$sp[i]) + 0.45, plot_data$cumulative_total[i],
       col = adjustcolor(plot_data$color[i], alpha.f = plot_data$alpha[i]),
       border = NA)
}

# Add species names
text(x = 1:length(unique(plot_data$sp)), y = par("usr")[3] - 0.1, 
     labels = unique(plot_data$sp), 
     srt = 45, adj = 1, xpd = TRUE, cex = 0.8)

# Add legend for categories
legend("topright", legend = names(category_colors), 
       fill = category_colors, 
       title = "Category", 
       cex = 0.8, 
       inset = c(-0.2, 0), 
       xpd = TRUE)

# Create color swatches for drivers
unique_alphas <- sort(unique(driver_alphas))
driver_colors <- sapply(unique_alphas, function(a) adjustcolor("grey", alpha.f = a))

# Add legend for drivers
legend("right", legend = paste("Alpha", unique_alphas), 
       fill = driver_colors, 
       title = "Driver Alpha Levels", 
       cex = 0.8, 
       inset = c(-0.2, 0.5), 
       xpd = TRUE)

# Close the device
dev.off()

print("Plot has been saved as 'base_r_stacked_bar_chart_ordered_alphas.png'")

# Print the driver information dataframe
print("Driver information dataframe:")
print(driver_info_df)

# If you want to save this dataframe for future use:
# write.csv(driver_info_df, "driver_info.csv", row.names = FALSE)

# - # - # ---------------------------------  trying the base plot function
library(dplyr)
library(tidyr)
library(ggplot2)

# Read the data
sumsts <- read.csv("csvs/sumsts_tr.csv")

# Data preparation
bplot <- sumsts %>%
  select(-min, -max, -sd) %>%
  mutate(sp = gsub("_", " ", sp))

# Add bird type information
# changing eider and LTD to shorebird
bird_types <- c(
  "American Golden-Plover" = "Shorebird",
  "Bairds Sandpiper" = "Shorebird",
  "Black-bellied Plover" = "Shorebird",
  "Buff-breasted Sandpiper" = "Shorebird",
  "Cackling Goose" = "Waterfowl",
  "King Eider" = "Waterfowl",
  "Long-tailed Duck" = "Waterfowl",
  "Pectoral Sandpiper" = "Shorebird",
  "Red-throated Loon" = "Waterfowl",
  "Red Knot" = "Shorebird",
  "Ruddy Turnstone" = "Shorebird",
  "Snow Goose" = "Waterfowl",
  "Tundra Swan" = "Waterfowl",
  "White-rumped Sandpiper" = "Shorebird"
)

bplot <- bplot %>%
  mutate(bird_type = bird_types[sp])

# Data aggregation
di <- bplot %>%
  group_by(sp, driver, category, bird_type) %>%
  summarise(mean_sum = sum(mean, na.rm = TRUE), .groups = "drop") %>%
  group_by(sp) %>%
  mutate(species_total = sum(mean_sum)) %>%
  ungroup() %>%
  arrange(desc(species_total), bird_type, desc(mean_sum)) %>%
  mutate(
    sp = factor(sp, levels = unique(sp)),
    category = factor(category, levels = c("Urban Expansion", "Marine pollution", "Direct Human Presence", "Agriculture"))
  )

# Color scheme
bird_type_colors <- c("Shorebird" = "#FF9999", "Waterfowl" = "#66B2FF")
category_colors <- c("Urban Expansion" = "#FF9999", "Marine pollution" = "#66B2FF",
                     "Direct Human Presence" = "#99FF99", "Agriculture" = "#FFCC99")

# Create the plot
p <- ggplot(di, aes(x = sp, y = mean_sum, fill = category, alpha = driver)) +
  geom_col(position = "stack", width = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  facet_grid(. ~ bird_type, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = category_colors) +
  scale_alpha_manual(values = seq(0.4, 1, length.out = n_distinct(di$driver))) +
  theme_minimal(base_family = "serif") +
  theme(
    text = element_text(family = "serif"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Increased size
    axis.text.y = element_text(size = 10),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(family = "serif", size = 16),
    axis.title = element_text(family = "serif", size = 14),
    legend.title = element_text(family = "serif", size = 12),
    legend.text = element_text(family = "serif", size = 10),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold", size = 14)  # Increased size
  ) +
  labs(x = NULL, y = "Impact Score", fill = "Category", alpha = "Driver")

# Display the plot
print(p)

# Optionally, save the plot
ggsave("stacked_bar_plot_by_bird_type.pdf", p, width = 15, height = 10, units = "in")





# - # - # ---------------------------------  trying the base plot function
# duplicating script for difference in waterfowl v shorebird, then negative v positive
library(dplyr)
library(tidyr)
library(ggplot2)

# Read the data
sumsts <- read.csv("csvs/sumsts_tr.csv")

# Data preparation
bplot <- sumsts %>%
  select(-min, -max, -sd) %>%
  mutate(sp = gsub("_", " ", sp))

# Add bird type information
# changing eider and LTD to shorebird
bird_types <- c(
  "American Golden-Plover" = "Negative trends",
  "Bairds Sandpiper" = "Negative trends",
  "Black-bellied Plover" = "Negative trends",
  "Buff-breasted Sandpiper" = "Negative trends",
  "Cackling Goose" = "Positive trends",
  "King Eider" = "Negative trends",
  "Long-tailed Duck" = "Negative trends",
  "Pectoral Sandpiper" = "Negative trends",
  "Red-throated Loon" = "Positive trends",
  "Red Knot" = "Negative trends",
  "Ruddy Turnstone" = "Negative trends",
  "Snow Goose" = "Positive trends",
  "Tundra Swan" = "Positive trends",
  "White-rumped Sandpiper" = "Negative trends"
)

bplot <- bplot %>%
  mutate(bird_type = bird_types[sp])

# Data aggregation
di <- bplot %>%
  group_by(sp, driver, category, bird_type) %>%
  summarise(mean_sum = sum(mean, na.rm = TRUE), .groups = "drop") %>%
  group_by(sp) %>%
  mutate(species_total = sum(mean_sum)) %>%
  ungroup() %>%
  arrange(desc(species_total), bird_type, desc(mean_sum)) %>%
  mutate(
    sp = factor(sp, levels = unique(sp)),
    category = factor(category, levels = c("Urban Expansion", "Marine pollution", "Direct Human Presence", "Agriculture"))
  )

# Color scheme
bird_type_colors <- c("Negative trends" = "#FF9999", "Positive trends" = "#66B2FF")
category_colors <- c("Urban Expansion" = "#FF9999", "Marine pollution" = "#66B2FF",
                     "Direct Human Presence" = "#99FF99", "Agriculture" = "#FFCC99")

# Create the plot
p <- ggplot(di, aes(x = sp, y = mean_sum, fill = category, alpha = driver)) +
  geom_col(position = "stack", width = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  facet_grid(. ~ bird_type, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = category_colors) +
  scale_alpha_manual(values = seq(0.4, 1, length.out = n_distinct(di$driver))) +
  theme_minimal(base_family = "serif") +
  theme(
    text = element_text(family = "serif"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Increased size
    axis.text.y = element_text(size = 10),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(family = "serif", size = 16),
    axis.title = element_text(family = "serif", size = 14),
    legend.title = element_text(family = "serif", size = 12),
    legend.text = element_text(family = "serif", size = 10),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold", size = 14)  # Increased size
  ) +
  labs(x = NULL, y = "Impact Score", fill = "Category", alpha = "Driver")

# Display the plot
print(p)

# Optionally, save the plot
ggsave("stacked_bar_plot_by_trend.pdf", p, width = 15, height = 10, units = "in")




