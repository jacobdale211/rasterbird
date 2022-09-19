# If this code is used to crop the bird range, then I would do this in the script /code/data/02-data-crop.R, rather than here. 
#placeholder cropping of brange
brange_small = brange[1:1,]
plot(brange_small)
st_area(brange_small)