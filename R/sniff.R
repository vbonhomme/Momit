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
sniff <- function(path, pattern=NULL){
  # messaging where we are looking at
  paste0("sniffing files in ", crayon::green(paste0(here::here(), path))) %>%
    cli::cli_alert_info()

  path %>%
    # grab paths and useful infos
    fs::dir_info(recurse = TRUE, type = "file", regexp=pattern) %>%
    # mutate useful bits
    dplyr::mutate(filename = .data$path %>% stringr::str_split("/") %>% purrr::map_chr(~.x[length(.x)]), # take last
                  id       = .data$filename %>% stringr::str_remove("\\..*$"), # remove from dot to the end
                  ext      = .data$filename %>% stringr::str_remove("^.*\\."), # remove from begin to the dot
                  folder   = .data$path %>% stringr::str_split("/") %>% purrr::map_chr(~.x[length(.x)-1]) # containing folder
    ) %>%
    # add count id
    dplyr::add_count(.data$id) %>%
    # drop most created by fs::dir_info
    dplyr::select(.data$n, .data$id, .data$folder, .data$filename, .data$ext, .data$size, .data$path,) %>%
    # arrange a bit
    dplyr::arrange(.data$id, .data$ext, .data$folder) -> res

  # header with number of items and size
  cli::cli_alert_success("Found {nrow(res)} items. Total size {sum(res$size)}:")
  return(res)
}
#
# sniff("inst/extdata/")
#
# install.packages("here")

# "~/Research/2020-DeepMorphometrics/data_PSL/ready/" %>% sniff()

# "~/Research/2020-DeepMorphometrics/data/PSL/" %>% sniff() %>%
#   dplyr::mutate(to=glue("plop/{ext}"))
#

