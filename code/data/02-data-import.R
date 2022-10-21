brange <- sf::st_read("./data/data-raw/birdlife/birds_multistress/Bylot_non_breeding_range.shp")

brange_small = brange[1,1,]
#halpern
path_halpern <- dir("./data/data-raw/halpern_cea-4f84f0e3/", pattern = ".tif", full.names = TRUE)

dat_halpern <- lapply(path_halpern, stars::read_stars, proxy = TRUE)
for (i in 1:length(dat_halpern)) {
  dat_halpern[[i]] <- stars::read_stars(path_halpern[i])
}

# #venter
# path_venter <- dir("./data/data-raw/venter/Dryadv3/Maps/", pattern = ".tif$", full.names = TRUE)
# 
# dat_venter <- lapply(path_venter, stars::read_stars, proxy = TRUE)
# for (i in 1:length(path_venter)) {
#   dat_venter[[i]] <- stars::read_stars(path_venter[i])
# }

stress <- c(dat_halpern, dat_venter)

#Loads data but loads ALL .tif files, not selected ones - do I add the object with
# the list of selected .tif names to the lapply function?
