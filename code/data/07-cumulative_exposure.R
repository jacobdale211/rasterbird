source("./code/functions/check.R")
plt <-"figures/cumulative/"
chk_create(plt)

str <- "figures/stressors/"
chk_create(str)

ras <- "./data/data-format/bird-grid/"
files <- dir(ras, pattern = ".tif$", full.names = TRUE)
bgrid <- lapply(files, stars::read_stars)

# Names for plots
cumul_nam <- lapply(bgrid, names)
cumul_nam <- gsub("bird-grid_", "cumulative_", cumul_nam) 
cumul_nam <- gsub(".tif", "", cumul_nam) 

# Get bird names
spnm <- lapply(bgrid, names)
spnm <- gsub("bird-grid_", "", spnm) 
spnm <- gsub(".tif", "", spnm) 

library(stars)
birdfolders <- dir("data/data-format/bird-transformed", full.names = TRUE)

for(i in 1:length(birdfolders)) {
  input <- cumul_nam[[i]]
  
  dat <- dir(
    birdfolders[i],
    pattern = ".tif$", 
    full.names = TRUE
  ) |>
  lapply(stars::read_stars, proxy = FALSE)
  
  # Cumulative plots of everything 
  cumul <- do.call("c", dat) |>
           st_redimension() |>
           st_apply(c(1,2), sum, na.rm = TRUE)
<<<<<<< HEAD
  # Export
  png(file = glue::glue("figures/cumulative/{input}.png"))
 image(cumul, col = viridis::viridis(100))
  dev.off()
=======
  # plot(cumul, breaks = "equal")
  image(cumul, col = viridis::viridis(100))
>>>>>>> e54e0d3c3f2822c5d348972685520437c17c4f79
}
  