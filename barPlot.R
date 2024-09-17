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

shpg <- values(res, species = sp, value_column = ship)
shpg$driver <- "Shipping"
write.csv(shpg, "ship16.csv")

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
                   ino, inva, ocn, shpg,
                   lhal, lvent, phal, pvent,
                   fert, pest, pstr, crop)
# adding category based on driver
assign_category <- function(x) {
  ifelse(x %in% c("Urban Environments", "Waterways", "Railways", "Roads"), "Urban Expansion",
         ifelse(x %in% c("Inorganic pollution", "Invasive species", "Ocean pollution", "Shipping"), "Marine pollution",
                ifelse(x %in% c("Fertilizer plumes", "Pesticide plumes", "Pastures", "Croplands"), "Agriculture",
                "Direct Human Presence")))
  }
all_stats$category <- assign_category(all_stats$driver)
write.csv(all_stats, "sumsts.csv")

# creating species tables
sumsts <- read.csv("sumsts.csv")
spl <- split(sumsts, sumsts$sp)
for (i in seq_along(spl)) {
  species_name <- names(spl)[i]
  df <- spl[[i]]
  
  write.csv(df, file = paste0("df_species_", species_name, ".csv"), row.names = FALSE)
}

# # # # #
library(dplyr)
sumsts <- read.csv("sumsts.csv")


# sorting out tables to be species only
filter_dataframe <- function(df, column, value) {
  df %>% filter(!!sym(column) == value)
}

amgplo <- filter_dataframe(sumsts, "sp", "American_Golden-Plover")
# # # # # # # #







# categories
# urbev <- read.csv("urbev.csv")
# marpol <- read.csv("marpol.csv")
# dhp <- read.csv("dhp.csv")
# agr <- read.csv("agr.csv")


# # # # # #
library(dplyr)
library(tidyr)
library(ggplot2)



sumsts <- read.csv("sumsts.csv")
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



# plot with preliminary legend
driver_alphas <- setNames(seq(0.4, 1, length.out = n_distinct(di$driver)), 
                          unique(di$driver))

# Create a data frame that maps drivers to categories
driver_category_map <- di %>%
  select(driver, category) %>%
  distinct()

p <- ggplot(di, aes(x = sp, y = mean_sum, fill = category, alpha = driver)) +
  geom_col(position = "stack", width = 0.7) +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  scale_fill_manual(values = category_colors) +
  scale_alpha_manual(
    values = driver_alphas,
    guide = guide_legend(
      override.aes = list(
        fill = sapply(names(driver_alphas), function(d) {
          category_colors[driver_category_map$category[driver_category_map$driver == d]]
        }),
        alpha = 1  # Set alpha to 1 in legend to show full color
      )
    )
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    text = element_text(family = "serif"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(x = NULL, y = "Impact Score", fill = "Category", alpha = "Driver")

# Display the plot
print(p)

