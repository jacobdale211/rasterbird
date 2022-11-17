#halpern
path_halpern <- dir("./data/data-raw/halpern_cea-4f84f0e3/", pattern = ".tif", full.names = TRUE)
dat_halpern <- lapply(path_halpern, stars::read_stars, proxy = TRUE)
nm <- lapply(dat_halpern, function(x) tools::file_path_sans_ext(names(x)))
names(dat_halpern) <- nm

#venter
path_venter <- dir(
  "./data/data-raw/terrestrial_human_footprint_venter-103a233e/", 
  pattern = ".tif$", 
  full.names = TRUE
)
dat_venter <- lapply(path_venter, stars::read_stars, proxy = TRUE)
nm <- lapply(dat_venter, function(x) tools::file_path_sans_ext(names(x)))
names(dat_venter) <- nm
stress <- c(dat_halpern, dat_venter)

#Loads data but loads ALL .tif files, not selected ones - do I add the object with
# the list of selected .tif names to the lapply function?
