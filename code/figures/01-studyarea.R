brange <- st_transform(brange, crs = st_crs(stressor))
ext <- st_bbox(brange)
stressor <- st_crop(stressor, ext)

#grid
brange_buffer <- st_buffer(brange, 10000)
grid <- st_make_grid(brange_buffer, n = c(20,20))
uid <- st_intersects(brange_buffer, grid) |> unlist() |> sort()
uid
grid <- grid[uid]
