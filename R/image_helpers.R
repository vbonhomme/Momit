#' Add helpers for working with image-magick
#'
#' @param x `image-magick` object
#'
#' @name image_helpers
NULL

#' @describeIn image_helpers tests if is image-magick
#' @export
is_image <- function(x){
  inherits(x, "magick-image")
}

#' @describeIn image_helpers converts image-magick to 2D raster matrix
#' @export
image_to_raster <- function(x){
  if (is_image(x)){
    x <- as.integer(x[[1]])
  }
  if (length(dim(x))>2)
    x[,,]
}

#' @describeIn image_helpers returns image center coordinates
#' @export
image_center <- function(x){
  # magick case
  if (is_image(x)){
  x %>%
    magick::image_info() %>%
    dplyr::select(.data$width, .data$height) %>%
    unlist() -> d
  }
  # matrix or array case
  if (is.array(x)){
    dim(x)[1:2] %>%
      `names<-`(c("width", "height")) -> d
  }
  # ceiling is for (only theoretical) dim(1, 1) case
  ceiling(d/2)
}

