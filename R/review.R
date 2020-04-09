#' Review files
#'
#' Interactive review of images (if present) and acquired informations
#'
#' @param x [Momocs2::mom] object, typically returned by [sniff] here
#'
#' @export
review <- function(x){
  UseMethod("review")
}

#' @rdname review
#' @export
review.default <- function(x){
  .msg_info("review: no method defined for this class")
}

# will probably allow it for mom_tbl
#' @rdname review
#' @export
review.sniff_tbl <- function(x){
  # check for paths
  # checks for images in path
  repeat {
    readline(prompt = "Press <Enter> to continue, <Esc> to quit...")
    path <- x %>% slive() %>% dplyr::pull(path)

    if (is_imagefile(path)){
      paste0("Viewing: ", path) %>% cli::cat_line()
      path %>% magick::image_read() %>% print()
    } else {
      paste0(path, " not an image; skipping") %>% .msg_info()
      next()
    }
  }
}


