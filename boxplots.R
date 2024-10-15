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
# All species all drivers

# Load required libraries
library(dplyr)
library(tidyr)

# Read the CSV file
data <- read.csv("sumsts_tr.csv", stringsAsFactors = FALSE)

# Prepare the data
plot_data <- data %>%
  group_by(sp, driver) %>%
  summarise(
    lower = mean - sd,
    middle = mean,
    upper = mean + sd,
    ymin = min,
    ymax = max,
    perc = first(perc),
    type = first(type)
  ) %>%
  ungroup()

# Set up the plot
pdf("all_species_all_drivers_boxplot.pdf", width = 20, height = 30)  # Adjust size as needed
par(mar = c(10, 20, 5, 2) + 0.1, mfrow = c(7, 2))  # 7 rows, 2 columns of plots

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
  
  # Add population trend as text
  text(1:nrow(driver_data), driver_data$ymin, 
       labels = round(driver_data$perc, 1), 
       pos = 1, cex = 0.6, col = "red")
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
