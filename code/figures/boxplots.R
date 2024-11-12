# Load required libraries
library(dplyr)

# Read the CSV file
data <- read.csv("sumsts_tr.csv", stringsAsFactors = FALSE)

# Filter and prepare the data for the "Direct Human Presence" category
direct_human_presence <- data %>%
  filter(category == "Direct Human Presence") %>%
  group_by(sp) %>%
  summarise(
    perc = first(perc),
    lower = mean(mean) - sd(mean),
    middle = mean(mean),
    upper = mean(mean) + sd(mean),
    ymin = min(min),
    ymax = max(max),
    type = first(type)
  ) %>%
  arrange(desc(perc))  # Sort by population trend

# Define colors
colors <- c("shorebird" = "lightblue", "waterfowl" = "lightgreen")
border_colors <- c("shorebird" = "blue", "waterfowl" = "darkgreen")

# Prepare the plot
pdf("direct_human_presence_boxplot.pdf", width = 12, height = 8)  # Adjust size as needed
par(mar = c(8, 4, 4, 2) + 0.1)  # Increase bottom margin for species names

plot(NULL, xlim = c(0.5, nrow(direct_human_presence) + 0.5), 
     ylim = range(c(direct_human_presence$ymin, direct_human_presence$ymax)),
     xlab = "", ylab = "Driver Values", main = "Species for Direct Human Presence",
     axes = FALSE)

# Add boxplots
for (i in 1:nrow(direct_human_presence)) {
  boxplot(list(c(direct_human_presence$lower[i], 
                 direct_human_presence$middle[i], 
                 direct_human_presence$upper[i])), 
          add = TRUE, at = i, 
          boxwex = 0.7, 
          col = colors[direct_human_presence$type[i]], 
          border = border_colors[direct_human_presence$type[i]], 
          whisklty = 1, 
          staplelty = 0, 
          outpch = NA)
  
  # Add whiskers
  segments(i, direct_human_presence$ymin[i], i, direct_human_presence$lower[i], 
           col = border_colors[direct_human_presence$type[i]])
  segments(i, direct_human_presence$upper[i], i, direct_human_presence$ymax[i], 
           col = border_colors[direct_human_presence$type[i]])
}

# Add axes and labels
axis(2)  # y-axis
axis(1, at = 1:nrow(direct_human_presence), labels = FALSE)
text(1:nrow(direct_human_presence), par("usr")[3] - 0.05 * diff(par("usr")[3:4]), 
     labels = direct_human_presence$sp, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)

# Add legend
legend("topright", legend = c("Shorebird", "Waterfowl"), 
       fill = c(colors["shorebird"], colors["waterfowl"]), 
       border = c(border_colors["shorebird"], border_colors["waterfowl"]))

# Close the PDF device
dev.off()

print("Boxplot PDF has been created.")

# Print summary statistics
print(direct_human_presence)



#--------------------------------------------------------------------------------------------
# Species per driver
# Load required libraries
library(dplyr)
library(tidyr)

# Read the CSV file
data <- read.csv("csvs/sumsts_tr.csv", stringsAsFactors = FALSE)

# Define species types and order explicitly
species_order <- c(
  "American_Golden-Plover",
  "Bairds_Sandpiper",
  "Black-bellied_Plover",
  "Buff-breasted_Sandpiper",
  "Cackling_Goose",
  "King_Eider",
  "Long-tailed_Duck",
  "Pectoral_Sandpiper",
  "Red-throated_Loon",
  "Red_Knot",              # Ensure Red_Knot comes before Red-throated_Loon
  "Ruddy_Turnstone",
  "Snow_Goose",
  "Tundra_Swan",
  "White-rumped_Sandpiper"
)

species_types <- c(
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

# Prepare the data
plot_data <- data %>%
  group_by(sp, driver) %>%
  summarise(
    lower = mean - sd,
    middle = mean,
    upper = mean + sd,
    ymin = min,
    ymax = max,
    perc = first(perc)
  ) %>%
  ungroup() %>%
  mutate(
    type = species_types[sp],
    sp = factor(sp, levels = species_order)  # Set explicit order of species
  )

# Set up the plot
pdf("all_species_all_drivers_boxplot-alphabetical.pdf", width = 20, height = 30)
par(mar = c(10, 20, 5, 2) + 0.1, mfrow = c(7, 2))

# Define colors
colors <- c("shorebird" = "lightblue", "waterfowl" = "lightgreen")
border_colors <- c("shorebird" = "blue", "waterfowl" = "darkgreen")

# Create boxplots for each driver
for (current_driver in unique(plot_data$driver)) {
  driver_data <- plot_data %>% filter(driver == current_driver)
  
  # Create the boxplot
  boxplot(middle ~ sp, data = driver_data, 
          ylim = range(c(driver_data$ymin, driver_data$ymax)),
          main = current_driver,
          xlab = "",
          ylab = "Values",
          las = 2,  # Rotate x-axis labels
          cex.axis = 0.7,  # Reduce axis label size
          col = colors[driver_data$type],
          border = border_colors[driver_data$type],
          outline = FALSE)  # Don't plot outliers
  
  # Add whiskers and boxes
  for (i in 1:nrow(driver_data)) {
    lines(c(i, i), c(driver_data$ymin[i], driver_data$ymax[i]), 
          col = border_colors[driver_data$type[i]])
    rect(i - 0.3, driver_data$lower[i], i + 0.3, driver_data$upper[i], 
         col = colors[driver_data$type[i]], 
         border = border_colors[driver_data$type[i]])
  }
}

# Add an overall legend
plot(NULL, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
legend("center", legend = c("Shorebird", "Waterfowl"), 
       fill = c(colors["shorebird"], colors["waterfowl"]), 
       border = c(border_colors["shorebird"], border_colors["waterfowl"]),
       title = "Species Type")

# Close the PDF device
dev.off()
print("Boxplot PDF has been created.")






#--------------------------------------------------------------------------------------------
# Drivers per species
# Load required libraries
library(dplyr)
library(tidyr)

# Read the CSV file
data <- read.csv("sumsts_tr.csv", stringsAsFactors = FALSE)

# Prepare the data
plot_data <- data %>%
  group_by(sp, driver, category) %>%
  summarise(
    lower = mean - sd,
    middle = mean,
    upper = mean + sd,
    ymin = min,
    ymax = max,
    perc = first(perc)
  ) %>%
  ungroup()

# Define colors for categories
category_colors <- c("Urban Expansion" = "#FF9999",
                     "Marine pollution" = "#66B2FF",
                     "Direct Human Presence" = "#99FF99",
                     "Agriculture" = "#FFCC99")

# Set up the PDF file
pdf("individual_species_all_drivers_boxplot.pdf", width = 15, height = 10)

# Create boxplots for each species
for (current_species in unique(plot_data$sp)) {
  species_data <- plot_data %>% filter(sp == current_species)
  
  # Set up the plot
  par(mar = c(10, 4, 4, 2) + 0.1)
  
  # Create the boxplot
  boxplot(middle ~ driver, data = species_data, 
          ylim = range(c(species_data$ymin, species_data$ymax)),
          main = paste("Drivers for", current_species),
          xlab = "",
          ylab = "Values",
          las = 2,  # Rotate x-axis labels
          cex.axis = 0.7,  # Reduce axis label size
          col = category_colors[species_data$category],
          border = "gray",
          outline = FALSE)  # Don't plot outliers
  
  # Add whiskers and boxes
  for (i in 1:nrow(species_data)) {
    lines(c(i, i), c(species_data$ymin[i], species_data$ymax[i]), col = "gray")
    rect(i - 0.3, species_data$lower[i], i + 0.3, species_data$upper[i], 
         col = category_colors[species_data$category[i]], 
         border = "gray")
  }
  
  # Add a horizontal line for the population trend
  abline(h = unique(species_data$perc), col = "red", lty = 2)
  text(1, unique(species_data$perc), paste("Population Trend:", round(unique(species_data$perc), 2), "%"), 
       pos = 3, col = "red", cex = 0.8)
  
  # Add legend
  legend("topright", legend = names(category_colors), 
         fill = category_colors, 
         border = "gray",
         title = "Categories",
         cex = 0.7)
}

# Close the PDF device
dev.off()

print("Individual species boxplots PDF has been created.")












#--------------------------------------------------------------------------------------------

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Read the CSV file
data <- read.csv("csvs/sumsts_tr.csv")

# Define species categories
species_categories <- c(
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

# Data preparation
plot_data <- data %>%
  mutate(sp = gsub("_", " ", sp)) %>%
  select(sp, mean, perc) %>%
  mutate(category = species_categories[sp])

# Calculate median exposure for sorting
species_median <- plot_data %>%
  group_by(sp) %>%
  summarise(median_exposure = median(mean, na.rm = TRUE)) %>%
  arrange(desc(median_exposure))

# Order species by median exposure
plot_data$sp <- factor(plot_data$sp, levels = species_median$sp)

# Define colors for categories
category_colors <- c("Shorebird" = "#ADD8E6", "Waterfowl" = "#90EE91")

# Create the plot
p <- ggplot(plot_data, aes(x = sp, y = mean, fill = category)) +
  geom_boxplot(outlier.shape = 1, outlier.size = 1) +
  scale_fill_manual(values = category_colors) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  labs(
    title = "Distribution of Overall Mean Exposure Across Species",
    x = "Species",
    y = "Mean Exposure",
    fill = "Species Category"
  ) +
  coord_flip()  # Flip coordinates for horizontal boxplot

# Display the plot
print(p)

# Save the plot (optional)
ggsave("all_species_mean_exposure_boxplot_categories.pdf", p, width = 12, height = 8, units = "in")

# Print summary statistics
summary_stats <- plot_data %>%
  group_by(sp, category) %>%
  summarise(
    median_exposure = median(mean, na.rm = TRUE),
    min_exposure = min(mean, na.rm = TRUE),
    max_exposure = max(mean, na.rm = TRUE),
    population_trend = first(perc)
  ) %>%
  arrange(desc(median_exposure))

print(summary_stats)



#-----------------------------------------------------------------------
# boxplots with different order -- based on levels of exposure
# Load required libraries
# Load required libraries
library(dplyr)
library(tidyr)

# Read the CSV file
data <- read.csv("csvs/sumsts_tr.csv", stringsAsFactors = FALSE)

# Define species types
species_types <- c(
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

# Prepare the data
plot_data <- data %>%
  group_by(sp) %>%
  mutate(type = species_types[first(sp)])

# Set up the plot
pdf("all_species_all_drivers_boxplot.pdf", width = 20, height = 30)
par(mar = c(10, 20, 5, 2) + 0.1, mfrow = c(7, 2))

# Define colors
colors <- c("shorebird" = "lightblue", "waterfowl" = "lightgreen")
border_colors <- c("shorebird" = "blue", "waterfowl" = "darkgreen")

# Create boxplots for each driver
for (current_driver in unique(plot_data$driver)) {
  driver_data <- plot_data %>% 
    filter(driver == current_driver)
  
  # Get species order for this specific driver
  species_order <- driver_data %>%
    group_by(sp) %>%
    summarize(mean_exposure = mean(mean)) %>%
    arrange(desc(mean_exposure)) %>%
    pull(sp)
  
  # Create empty plot
  plot(NULL, 
       xlim = c(0.5, length(unique(driver_data$sp)) + 0.5),
       ylim = range(c(driver_data$min, driver_data$max)),
       xlab = "",
       ylab = "Values",
       main = current_driver,
       xaxt = "n")
  
  # Add species names
  axis(1, at = 1:length(species_order), 
       labels = species_order, 
       las = 2, 
       cex.axis = 0.7)
  
  # Add boxes for each species
  for (i in seq_along(species_order)) {
    sp_data <- driver_data %>% filter(sp == species_order[i])
    if(nrow(sp_data) > 0) {
      sp_type <- species_types[species_order[i]]
      
      # Draw box
      rect(i - 0.3, sp_data$mean - sp_data$sd,
           i + 0.3, sp_data$mean + sp_data$sd,
           col = colors[sp_type],
           border = border_colors[sp_type])
      
      # Draw median line
      segments(i - 0.3, sp_data$mean, 
               i + 0.3, sp_data$mean,
               col = border_colors[sp_type],
               lwd = 2)
      
      # Draw whiskers
      segments(i, sp_data$mean - sp_data$sd, 
               i, sp_data$min,
               col = border_colors[sp_type])
      segments(i, sp_data$mean + sp_data$sd, 
               i, sp_data$max,
               col = border_colors[sp_type])
    }
  }
  
  # Add grid
  grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")
}

# Add legend
plot(NULL, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
legend("center", 
       legend = c("Shorebird", "Waterfowl"),
       fill = c(colors["shorebird"], colors["waterfowl"]),
       border = c(border_colors["shorebird"], border_colors["waterfowl"]),
       title = "Species Type")

# Close the PDF device
dev.off()

print("Boxplot PDF has been created.")
