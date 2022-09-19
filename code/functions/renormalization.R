# This is a function, put it in code/functions/
quantNorm <- function(x) {
  id <- x != 0
  x <- x / quantile(x[id], probs = .99, na.rm = T)
  x[x > 1] <- 1
  x[x < 0] <- 0
  x
}