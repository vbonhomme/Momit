
# sniff ---------------------------------------------------
#' Find files
#'
#' Find files in `path` location, and return a useful `tibble`. You can refine with a pattern.
#'
#'
#' @param path where to find item
#' @param pattern a [regexp] passed to [fs::dir_info]
#'
#' @export
sniff <- function(path=here::here(), pattern=NULL){
  # messaging where we are looking at
  paste0("sniffing files in ", crayon::green(path)) %>% cli::cli_alert_info()

  # here we go
  path %>%
    # grab paths and useful infos
    fs::dir_info(recurse = TRUE, type = "file", regexp=pattern) %>%
    # only retain path and size
    dplyr::select(.data$size, .data$path) %>%
    dplyr::mutate(path_dir = .data$path %>% extract_dir(),
                  file     = .data$path %>% extract_file(),
                  ext      = .data$path %>% extract_ext()) %>%
    tibble::as_tibble() %>%
    Momocs2::new_mom() %>%
    .append_class("sniff_tbl")
}

# printer methods add umber of files and size (if retained)
print.sniff_tbl <- function(x){
  NextMethod(print, x)

  # if size column, add it
  sizes <- x %>% dplyr::select_if(~class(.x)[1]=="fs_bytes")
  if (ncol(sizes)>0){
    size_sum <- sizes %>% dplyr::select(1) %>% dplyr::pull() %>% sum() %>% paste0("(", ., ")")
  } else {
    size_sum <- ""
  }

  # add the number of files
  cli::cli_alert_success("{nrow(x)} files {size_sum}")
}


#
# coo_close <- function(x){
#   x <- x[-1, ]
#   rbind(x, x[1,, drop=FALSE])
# }



