#test
# You could name this file 01-data-normalization.R and normalize your data here, then export it.
# In pseudo code: 

# Import halpern & venter data 
# Normalize data 
# Export data

halpern <- "4f84f0e3" 
venter <- "103a233e" 
pipedat::pipedat(c(halpern, venter)) 

#format data (select specific .tif's)
halpern_names <- list(night_lights)


#normalize data
quantNorm()


