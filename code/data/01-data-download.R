# COMMENT: 
# This script does not download the data, it imports them to R for use in other analyses.
# It means that there is a step missing, that of downloading the data directly from the web
# In case the data is not downloaded from the web (e.g. bird data), I would still document it and provide information. 
# You can either have individual scripts for each dataset, or perform all the downloads in a single 
# script, it's up to you. 
# In the end, though, this code here should either be part of individual scripts, meaning that you 
# use it when you need to work with the data in a script, or you use it to import the data. 
# In that case I would modify the name of the file to 02-data-import.R

# I will move this data 02-data-crop.R, but comment on it here
path <- dir("./data/data-raw/halpern/", pattern = ".tif", full.names = TRUE)
halpern <- lapply(path, read_stars, proxy = TRUE) # change data name to be more specific if you are importing multiple files 
venter <- stars::read_stars("./data/data-raw/venter/Dryadv3/Maps/croplands2005.tif")
#summary(venter) # do not run this type of code in a script unless you are using elements of the summary in the script. It's for exploration only. Either remove it, or put it as a comment if you want to have it ready for use. 

# What I would expect to see to make this reproducible 
halpern <- "4f84f0e3" # Global cumulative human impacts assessments in 2008 and 2013 
venter <- "103a233e" # Terrestrial human footprint
pipedat::pipedat(c(halpern, venter)) # Download data using pipedat package

# This will download more data than what you actually need, but you can either create a seperate script called something like 02-data-format.R to format and select only the data that you wish to use, or do it in your 02-data-crop.R script. In both cases, you would then export your formatted data in data-format by line of code, not manually.

# You could also use `file.remove()` to remove the .tif files that you do not need, directly in this script here. This is what I would do personally. 