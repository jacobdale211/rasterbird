# setwd("/Users/jacobdale/rasterbird")

drivers <- dir(
  here::here("data/data-stressors/"),
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

# adding americas map for halpern data
library(rnaturalearth)
america_poly <- sf::st_read("map.geojson")
world <- ne_countries(scale = "medium", returnclass = "sf")
std_ar <- subset(world, continent %in% c("North America", "South America"))
std_ar <- sf::st_crop(std_ar, america_poly)

#halpern
 for (i in 1:8) {
   input <- names[[i]]
   x <- stressors[[i]]
   x[x==0]<-NA
   p = log(x+1)
   p2 = log(p+1)
   p3 = log(p2+1)
   p4 = log(p3+1)
   
   png(file = glue::glue("figures/stressors/{input}.png"),width = 800, height = 600, units = "px", res = 150)
   par(bg = "#550c54")
   par(mar = c(0, 0, 0, 0))
   plot(sf::st_geometry(std_ar), col = "#3d1151", border = "#3d1151", 
        xlim = c(-130, -30), ylim = c(-60, 90))
  image(p4, col = viridis::viridis(900), add = TRUE, main = NULL)
  text(x = par("usr")[1], y = par("usr")[4], labels = names[i], col = "white", adj = c(-0.2, 1.9), font = 2)
  legendEGSL(range = c(0, 1), pal= viridis::viridis(100), cexMain = .75, cexSub = .5, n = 5)
  
  dev.off()
 }
#venter
for (i in 9:16) {
  input <- names[[i]]
  
  png(file = glue::glue("figures/stressors/{input}.png"),width = 800, height = 600, units = "px", res = 150)
  par(bg = "#550c54")
  par(mar = c(0, 0, 0, 0))
  image(stressors[[i]], col = viridis::viridis(900), main = NULL)
  text(x = par("usr")[1], y = par("usr")[4], labels = names[i], col = "white", adj = c(-0.2, 1.9), font = 2)
  legendEGSL(range = c(0, 1), pal= viridis::viridis(100), cexMain = .75, cexSub = .5, n = 5)
  
  dev.off()
}

# america_poly <- sf::st_read("map.geojson")

legendEGSL <- function (range = c(0,1),
                        pal = NULL,
                        cexMain = 1,
                        cexSub = .75,
                        mainTitle = NULL,
                        subTitle = NULL,
                        n = 5) {
  # Legends
  # Palette
  if(is.null(pal)) {
    pal <- colorRampPalette(slmetaPal('platform'))
  } else {
    if(class(pal) == 'character') {
      pal <- colorRampPalette(pal)
    }
  }
  
  # Determine plot boundaries, in units of the data
  xmin <- par("usr")[1]
  xmax <- par("usr")[2]
  ymin <- par("usr")[3]
  ymax <- par("usr")[4]
  xR <- xmax - xmin
  yR <- ymax - ymin
  
  xinit <- xmin + .034*xR # minimum left side to write
  yinit <- ymax - .056*yR # minimum upper side to write
  ygap <- .056*yR
  xgap <- .014*xR
  ybarUp <- yinit - ygap/2 - .0041*yR
  ybarDn <- yinit - ygap -ygap/2 + .0041*yR
  
  
  # Plot
  x <- seq(from = xinit, to = xinit + .17*xR, by = .0003*xR)
  z <- data.frame(y1 = ybarUp,
                  y2 = ybarDn,
                  x1 = x[1:length(x)-1],
                  x2 = x[2:length(x)],
                  col = pal(length(x)-1),
                  stringsAsFactors = F)
  for(k in 1:nrow(z)) {
    polygon(x = c(z$x1[k],z$x2[k],z$x2[k],z$x1[k],z$x1[k]),
            y = c(z$y1[k],z$y1[k],z$y2[k],z$y2[k],z$y1[k]),
            col = z$col[k],
            border = z$col[k])
  }
  
  # Add axis
  x <- seq(from = xinit, to = xinit + .17*xR, length.out = n)
  lines(x = c(xinit, xinit + .17*xR), y = rep(z$y2[1], 2))
  for(i in 1:n) lines(x = rep(x[i],2), y = c(z$y2[1], z$y2[1]- .003*yR))
  
  text(x = x,
       y =  rep(z$y2[1] - .0056*yR, n),
       labels = seq(from = floor(min(range[1])), to = ceiling(max(range[2])), length.out = n),
       cex = cexSub,
       col = "white",
       adj = c(.5, 1))
  
  # Add titles
  yText <- ybarUp + .028*yR
  
  # Add sub text
  if(!is.null(subTitle)) {
    text(x = xinit,
         y = yText,
         labels = subTitle,
         cex = cexSub,
         adj = c(0,1))
    yText <- yText + .0224*yR
  }
  
  # Add main title
  if(!is.null(mainTitle)) {
    text(x = xinit,
         y = yText,
         labels = mainTitle,
         cex = cexMain,
         font = 2,
         adj = c(0,1))
  }
}
legendEGSL(range = c(0, 1), pal= viridis::viridis(100), cexMain = .75, cexSub = .5, n = 5)
