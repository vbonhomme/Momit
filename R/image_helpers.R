#' Add helpers for working with image-magick
#'
#' @param x `image-magick` object
#' @param threshold to feed [magick::image_threshold]
#' @param canny_geometry to feed [magick::image_canny]
#'
#' @details For image_mask, some "missed" objects can
#' be avoided with large image files or by adjusting threshold
#' and canny_edge radius.
#'
#' @name image_helpers
#'
#' @examples
#' Momit_data("malus/malus1.jpg") %>%
#' # you dont need magick:: if your library(magick before)
#'   magick::image_read() -> m0
#'   m0
#'   m1 <- m0 %>% image_mask()
#'   m1
#'   mx <- m1 %>% image_explode
#'
#'   \dontrun{
#'   mx
#'   }
#'
#'   mx[1] %>% image_rasterize2D %>% algo_Conte()
NULL

#' @describeIn image_helpers tests if is of image-magick class
#' @export
is_image <- function(x){
  inherits(x, "magick-image")
}

#' @describeIn image_helpers converts image-magick to 2D raster matrix
#' @export
image_rasterize2D <- function(x){
  x %>%
     # magick::image_channel("Gray") %>%
    `[[`(1) %>%
    # turn into integers
    as.integer() %>%
    # and into a matrix (not a single slice array)
    `[`(,,1)
}

#' @describeIn image_helpers turn an inmage into a mask
#' @export
image_mask <- function(x, threshold="50%", canny_geometry = "0x1+10%+30%") {

  x %>%
    # extract lighness
    magick::image_channel("lightness") %>%
    # blur helps
    # magick::image_blur(radius = blur_radius, sigma = blur_sigma) %>%
    # binarize now
    magick::image_threshold(type="white", threshold = threshold) %>%
    # canny edges
    magick::image_canny(geometry = canny_geometry) %>%
    # black border to prevent border artifacts
    magick::image_border(color="black", geometry="20x20") %>%
    # fill with white
    magick::image_fill("white", fuzz=5) %>%
    # remove border now
    magick::image_chop(geometry="20x20") %>%
    magick::image_channel("lightness")
}

#' @describeIn image_helpers turn a multi-object image into multiple image with single object
#' @export
image_explode <- function(x){
  x %>%
    magick::image_connect(connectivity = 1) %>%
    magick::image_split() %>%
    # remove background layer
    `[`(-1) %>%
    magick::image_trim() %>%
    magick::image_border(color="white", geometry="2x2")
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

