stressor = stars::read_stars("./data/data-raw/halpern_cea-4f84f0e3/halpern_cea-4f84f0e3-2008-inorganic.tif/")

brange_small <- sf::st_transform(brange_small, crs = sf::st_crs(stressor))
ext <- sf::st_bbox(brange_small)
stressor <- sf::st_crop(stressor, ext)
