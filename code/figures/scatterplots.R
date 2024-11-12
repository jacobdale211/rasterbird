library(dplyr)
library(stringr)
brange <- sf::st_read("data/data-raw/birdlife/birds_multistress/Bylot_non_breeding_range.shp")

sp <- c(
  "American Golden-Plover",
  "Bairds Sandpiper",
  "Black-bellied Plover",
  "Buff-breasted Sandpiper",
  "Cackling Goose",
  "King Eider",
  "Long-tailed Duck",
  "Pectoral Sandpiper",
  "Red Knot",
  "Red-throated Loon",
  "Ruddy Turnstone",
  "Snow Goose",
  "Tundra Swan",
  "White-rumped Sandpiper"
)

brange <- brange %>%
  filter(str_detect(species, paste(sp, collapse = "|")))
plot(brange)



# lets look at that new data
wow <- sf::st_read("data/data-raw/venter/Anthromes-12k-DGG/an12_dgg_inputs.shp")
plot(wow)



























# scatterplot with line of best fit

# Load required libraries
# Load required libraries
library(ggplot2)
library(dplyr)

# Define species categories
species_categories <- c(
  "American_Golden-Plover" = "Shorebird",
  "Bairds_Sandpiper" = "Shorebird",
  "Black-bellied_Plover" = "Shorebird",
  "Buff-breasted_Sandpiper" = "Shorebird",
  "Cackling_Goose" = "Waterfowl",
  "King_Eider" = "Waterfowl",
  "Long-tailed_Duck" = "Waterfowl",
  "Pectoral_Sandpiper" = "Shorebird",
  "Red-throated_Loon" = "Waterfowl",
  "Red_Knot" = "Shorebird",
  "Ruddy_Turnstone" = "Shorebird",
  "Snow_Goose" = "Waterfowl",
  "Tundra_Swan" = "Waterfowl",
  "White-rumped_Sandpiper" = "Shorebird"
)

# Read and prepare data
data <- read.csv("csvs/sumsts_tr.csv", stringsAsFactors = FALSE) %>%
  group_by(sp) %>%
  summarise(
    total_exposure = sum(mean),
    trend = first(perc)
  ) %>%
  mutate(
    species_type = species_categories[sp],
    sp = gsub("_", " ", sp)
  )

# Create the plot
p<-ggplot(data, aes(x = total_exposure, y = trend, color = species_type, label = sp)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text(hjust = -0.1, vjust = 0, size = 3) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  scale_color_manual(values = c("Shorebird" = "#ADD8E6", "Waterfowl" = "#90EE91"),
                     name = "Species Type") +
  labs(title = "Total Mean Exposure vs Population Trends",
       x = "Cumulative Exposure Score",
       y = "Population Trend (%)") +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "top"
  )

print(p)


# scatterplot without line of best fit

# Load required libraries
library(ggplot2)
library(dplyr)

# Define species categories
species_categories <- c(
  "American_Golden-Plover" = "Shorebird",
  "Bairds_Sandpiper" = "Shorebird",
  "Black-bellied_Plover" = "Shorebird",
  "Buff-breasted_Sandpiper" = "Shorebird",
  "Cackling_Goose" = "Waterfowl",
  "King_Eider" = "Waterfowl",
  "Long-tailed_Duck" = "Waterfowl",
  "Pectoral_Sandpiper" = "Shorebird",
  "Red-throated_Loon" = "Waterfowl",
  "Red_Knot" = "Shorebird",
  "Ruddy_Turnstone" = "Shorebird",
  "Snow_Goose" = "Waterfowl",
  "Tundra_Swan" = "Waterfowl",
  "White-rumped_Sandpiper" = "Shorebird"
)

# Read and prepare data
data <- read.csv("csvs/sumsts_tr.csv", stringsAsFactors = FALSE) %>%
  group_by(sp) %>%
  summarise(
    total_exposure = sum(mean),
    trend = first(perc)
  ) %>%
  mutate(
    species_type = species_categories[sp],
    sp = gsub("_", " ", sp)
  )

# Create the plot
p<-ggplot(data, aes(x = total_exposure, y = trend, color = species_type)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(values = c("Shorebird" = "#72bcd4", "Waterfowl" = "#4ee44f"),
                     name = "Species Type") +
  labs(title = "Total Mean Exposure vs Population Trends",
       x = "Cumulative Exposure Score",
       y = "Population Trend (%)") +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "top"
  )
print(p)
