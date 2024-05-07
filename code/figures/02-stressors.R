drivers <- dir(
  here::here("output/data-stressors/"),
  pattern = ".tif$",
  full.names = TRUE
)
stressors <- lapply(drivers, stars::read_stars)

names <- c(
  "Inorganic pollution",
  "Invasive species",
  "Light pollution (Halpern)",
  "Ocean pollution",
  "Fertilizer plumes",
  "Pesticide plumes",
  "Population density (Halpern)",
  "Shipping",
  "Urban Environments",
  "Croplands",
  "Light pollution (Venter)",
  "Waterways",
  "Pastures",
  "Population density (Venter)",
  "Railways",
  "Roads")

path <- "figures/stressors"
 if (!dir.exists(path)) {
   dir.create(path, recursive = TRUE)
 }

#separated halpern and venter to make background colors different

#halpern
 for (i in 1:8) {
   input <- names[[i]]
   
   png(file = glue::glue("figures/stressors/{input}.png"))
   par(bg = "#430c54")
   par(mar = c(0, 0, 0, 0))
  image(stressors[[i]], col = viridis::viridis(900), main = NULL)
  text(x = par("usr")[1], y = par("usr")[4], labels = names[i], col = "white", adj = c(-0.2, 1.9), font = 2)
  dev.off()
 }
#venter
for (i in 9:16) {
  input <- names[[i]]
  
  png(file = glue::glue("figures/stressors/{input}.png"))
  par(bg = "#550c54")
  par(mar = c(0, 0, 0, 0))
  image(stressors[[i]], col = viridis::viridis(900), main = NULL)
  text(x = par("usr")[1], y = par("usr")[4], labels = names[i], col = "white", adj = c(-0.2, 1.9), font = 2)
  dev.off()
}

# america_poly <- sf::st_read("map.geojson")

 
