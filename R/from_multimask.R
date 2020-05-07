#' Import outline coordinates from multiple mask image file
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
#' @param x paths to `.txt` files
#' @param from_col,to_col column names where to get paths and where to store results
#' @param ... useless here
#' Useful for debugging.
#'
#' @return [Momocs2::coo_single]
#' @family import
#'
#' @examples
#' 1+2
#'
#' @export
import_multimask <- function(x, from_col, to_col, ...){
  UseMethod("import_mask")
}

#' @export
import_multimask <- function(x, ...){
  not_defined("import_multimask")
}

