library(stars)
birdfolders <- dir("data/data-format/bird-transformed", full.names = TRUE)

for(i in 1:length(birdfolders)) {
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
  # plot(cumul, breaks = "equal")
  image(cumul, col = viridis::viridis(100))
}
