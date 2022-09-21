stressor = dat_halpern[1]

brange_small <- stars::st_transform(brange_small, crs = sf::st_crs(stressor))
ext <- sf::st_bbox(brange_small)
stressor <- sf::st_crop(stressor, ext)
