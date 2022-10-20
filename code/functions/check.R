#' Check if folder and create if not
chk_create <- function(path) {
  if (!file.exists(path)) dir.create(path, recursive = TRUE)
}