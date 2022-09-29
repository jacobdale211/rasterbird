x=stressor
class(x)
x <- stars::st_as_stars(stressor)
class(x)
x2 = log(x+1)
image(x)
image(x2)
x3 <- x2
x3[[1]] <- quantNorm(x3[[1]])
image(x3)

#grid
brange_buffer <- sf::st_buffer(brange_small, 10000)
grid <- sf::st_make_grid(brange_buffer, n = c(20,20))
uid <- sf::st_intersects(brange_buffer, grid) |> unlist() |> sort()
uid
grid <- grid[uid]
