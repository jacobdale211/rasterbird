source("./code/functions/check.R")
source("./code/functions/legendEGSL.R")

plt <-"figures/cumulative/"
tif <-"output/cumulative"
chk_create(plt)
chk_create(tif)

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

# Red knot is i=9, snow goose is i=11

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
  
  # Export tif file
  stars::write_stars(cumul, glue::glue("output/cumulative/{input}.tif"))
  # Export
  par(mfrow = c(1,1))
  png(file = glue::glue("figures/cumulative/{input}.png"))
  image(cumul, col = viridis::viridis(100), main = spnm[i])
  legendEGSL(range = c(0, 1), pal= viridis::viridis(100), cexMain = .75, cexSub = .5, n = 5)
  dev.off()
}

# New folder - export each iteration of cumulative effect
# one tif file per species





