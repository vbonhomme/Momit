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
    x_i <- x %>% slive()
    path_i <- dplyr::pull(x_i, .data$path)
    if (is_imagefile(path_i)){
      paste0("Viewing: ", path_i) %>% cli::cat_line()
      # regular viewer
      path_i %>% magick::image_read() %>% print()

      # # classic viewer
      # gr <- path_i %>% magick::image_read() %>% magick::image_draw()
      # print(gr)
      # if (Momocs2::coo_nb(x_i)>0){
      #   coo <- Momocs2::pick(x_i, 1) %>% as.matrix()
      #   # print(coo)
      #   image_draw_outline <- function(x){
      #     polygon(x[, 1], x[, 2], border="red", lwd=2)
      #     points(x[1, 1], x[1, 2], pch=24, cex=2, col="red", bg="grey50")
      #   }
      #   image_draw_outline(coo)
      #   dev.off()
      #   print(gr)
      # }

    } else {
      paste0(path_i, " not an image; skipping") %>% .msg_info()
      next()
    }
  }
}
#
# "~/Research/2020-VitisRef/data/def_masks/" %>% sniff() %>%
#   dplyr::slice(1:10) %>% review()
