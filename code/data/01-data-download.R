

# I will move this data 02-data-crop.R, but comment on it here
path <- dir("./data/data-raw/halpern/", pattern = ".tif", full.names = TRUE)
halpern <- lapply(path, read_stars, proxy = TRUE) # change data name to be more specific if you are importing multiple files 
venter <- stars::read_stars("./data/data-raw/venter/Dryadv3/Maps/croplands2005.tif")
#summary(venter) # do not run this type of code in a script unless you are using elements of the summary in the script. It's for exploration only. Either remove it, or put it as a comment if you want to have it ready for use. 

# What I would expect to see to make this reproducible 
halpern <- "4f84f0e3" # Global cumulative human impacts assessments in 2008 and 2013 
venter <- "103a233e" # Terrestrial human footprint
pipedat::pipedat(c(halpern, venter)) # Download data using pipedat package
pipedat::pipedat("4f84f0e3", halpern_layers = c("inorganic","night_lights","invasives","plumes_fert","plumes_pest","population","shipping","ocean_pollution"))
# This will download more data than what you actually need, but you can either create a seperate script called something like 02-data-format.R to format and select only the data that you wish to use, or do it in your 02-data-crop.R script. In both cases, you would then export your formatted data in data-format by line of code, not manually.

# You could also use `file.remove()` to remove the .tif files that you do not need, directly in this script here. This is what I would do personally. 