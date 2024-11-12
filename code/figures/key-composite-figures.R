drivers <- dir(
  here::here("data/data-stressors/"),
  pattern = ".tif$",
  full.names = TRUE
)

stressors <- lapply(drivers, stars::read_stars)
image(stressors[[9]])

library(stars)
library(sf)
library(ggplot2)








# need to recheck formatting to get the rest of the birds... amg as test for now

data_dir <- "data/data-format/bird-transformed/American_Golden-Plover"
files <- list.files(data_dir, pattern = ".tif", full.names = TRUE)
amg_drivers <- lapply(files, stars::read_stars)

data_dir <- "data/data-format/bird-crop/American_Golden-Plover"
files <- list.files(data_dir, pattern = ".tif", full.names = TRUE)
amg_c <- lapply(files, stars::read_stars)
image(amg_c[[1]])
t <- na.omit(amg_c[[1]])
image(t)

data_dir <- "data/data-format/bird-warp/American_Golden-Plover"
files <- list.files(data_dir, pattern = ".tif", full.names = TRUE)
amg_w <- lapply(files, stars::read_stars)
image(amg_w[[1]])
t2 <- na.omit(amg_w[[1]])
image(t2)
plot(t2, breaks = "equal")



# stressor_names <- c(
#   "Built2009", "croplands2005", "2013-inorganic", "2013-invasives","2013-night_lights", "2013-ocean_pollution", "2013-plumes_fert",
#   "2013-plumes_pest", "2013-population", "2013-shipping","Lights2009","NavWater2009","Pasture2009","Popdensity2010", "Railways", "Roads")
image(amg_drivers[[1]])


par(mar = c(0, 0, 0, 0))

image(amg_drivers[[2]], col = viridis::rocket(100))
image(amg_drivers[[4]], col = viridis::rocket(100))
image(amg_drivers[[5]], col = viridis::rocket(100))
image(amg_drivers[[6]], col = viridis::rocket(100))
image(amg_drivers[[7]], col = viridis::rocket(100))
image(amg_drivers[[8]], col = viridis::rocket(100))
image(amg_drivers[[9]], col = viridis::rocket(100))
par(bg = "white")

image(amg_drivers[[10]], col = viridis::rocket(100))
image(amg_drivers[[12]], col = viridis::rocket(100))
image(amg_drivers[[16]], col = viridis::rocket(100))


amgplo <- stars::read_stars("output/cumulative/cumulative_American_Golden-Plover.tif")
amgplo <- amgplo * -1
image(amgplo, col = viridis::rocket(100))
plot(amgplo)








legend.composite <- function (range = c(0,1),
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
  
  xinit <- xmin + .30*xR # minimum left side to write
  yinit <- ymax - .1*yR # minimum upper side to write
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

