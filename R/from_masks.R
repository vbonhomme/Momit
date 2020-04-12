# utils ---------------------------------------------------
.if_array_average_to_matrix <- function(x){
  if (length(dim(x))==2)
    return(x)
  if (length(dim(x))>2)
    return((x[, , 1] + x[, , 2] + x[, , 3])/3)
  .msg_info("if_array_average_to_matrix: expect array with 3 dims, or a matrix")
}

.if_above_1_normalize_matrix <- function(x){
  if (any(x>1))
    x <- x/max(x)
  ## return
  x
}

.threshold_matrix <- function(x, k=0.5){
  x[x >  k] <- 1
  x[x <= k] <- 0
  x
}

#' Import outline coordinates from mask image file
#'
#' Given a path to an image mask,
#' returns a `[Momocs2::coo_single]` with xy outline coordinates.
#'
#' A mask (or silhouette) is ideally a grayscale image (8-bit)
#' containing a (pure) black shape onto a (pure) white background.
#'
#' This methods ensures all of this.
#'
#' @details
#' Calculations are delegated to `algo_Conte`.
#'
#' `.jpg` files take the `jpeg::readJPEG` shortcut (roughly twice faster).
#' Other files are processed with [magick].
#'
#' If one or more image does not work, try time_limit
#'
#' @param x path to an mask image
#' @param time_limit if this take more than this (in second) calculation is aborted.
#' Useful for debugging.
#' @return [Momocs2::coo_single]
#' @examples
#' 1+2
#'
#' @export
from_mask <- function(x, time_limit=5){
  UseMethod("from_mask")
}

#' @export
from_mask.mom_tbl <- function(x, time_limit=5){
  if (missing(x)) x <- sniff()
  f <- purrr::possibly(~ {
    pb$tick()
    from_mask(.x, time_limit=time_limit)
  },
  otherwise = Momocs2::new_coo_single())

  pb <- progress::progress_bar$new(
    format = "extracting outline :current/:total [:bar] :eta",
    total=nrow(x),
    width=60, clear=F)
  # progress::pb()$print()

  res <- dplyr::pull(x, .data$path) %>% purrr::map(f)
  res
}

#' @export
from_mask.character <- function(x, time_limit=5){
  setTimeLimit(time_limit)

  if (extract_ext(x)=="jpg")
    x <- jpeg::readJPEG(x) # twice faster for jpg
  else
    x <- magick::image_read(x)[[1]] %>% as.integer()

  # turn into a matrix by averaging three layers if required
  x %>% .if_array_average_to_matrix() %>%
    # normalize if required
    .if_above_1_normalize_matrix() %>%
    # thresoldize
    .threshold_matrix() %>%
    # finally run Conte
    algo_Conte() %>%
  # and return a coo_single
  Momocs2::coo_single()
}


# x <- sniff("~/Research/2020-Claudia/data/Modern/", pattern="jpg")
# #
# z <- x %>% from_mask()
#
# x %>% dplyr::mutate(coo=coo_list(z)) %>% review()
# #
# x <- sniff("~/Research/2020-VitisArch/data0/Arch_03_03_2020", pattern="NetB")
#
# x %>% from_mask()
#
# x %>% dplyr::pull(path) %>% purrr::map(from_mask.character)
