# path <- dir("./data/data-raw/halpern/", pattern = ".tif", full.names = TRUE)
# halpern <- lapply(path, read_stars, proxy = TRUE) # change data name to be more specific if you are importing multiple files 
# venter <- stars::read_stars("./data/data-raw/venter/Dryadv3/Maps/croplands2005.tif")

halpern <- "4f84f0e3" # Global cumulative human impacts assessments in 2008 and 2013 
venter <- "103a233e" # Terrestrial human footprint
pipedat::pipedat(venter) # Download data using pipedat package
pipedat::pipedat(halpern, halpern_layers = c("inorganic","night_lights","invasives","plumes_fert","plumes_pest","population","shipping","ocean_pollution"))

# You could also use `file.remove()` to remove the
#tif files that you do not need, directly in this script here. This is what I would do personally. 

# amgplo <- stars::read_stars("./data/data-raw/ebirdst/2020/amgplo/seasonal/amgplo_percent-population_full-year_max_lr_2020.tif/")
