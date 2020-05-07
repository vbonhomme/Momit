# from_txt

#' Import txt files
#'
#' Wraps `read.table`
#'
#' @param x paths to `.txt` files
#' @param from_col,to_col column names where to get paths and where to store results
#' @param ... additional arguments to feed [utils::read.table]
#'
#' @return [Momocs2::coo_single]
#' @family import
#' @examples
#' # list of files case
#' lf <- Momit_data("txt") %>% list_files()
#' lf
#' # single case
#' lf[1] %>% import_txt()
#' # multi case
#' lf %>% import_txt()
#'
#' # mom_tbl case
#' z <- Momit_data("txt") %>% sniff()
#' z %>% import_txt()
#'
#' # case with different from/to_col names
#' z %>%
#'   dplyr::rename(plop=path) %>%
#'   import_txt(from_col=plop, to_col=plip)

#' @export
import_txt <- function(x, from_col, to_col, ...){
  UseMethod("import_txt")
}

#' @export
import_txt.default <- function(x, ...){
  not_defined("import_txt")
}

#' @export
import_txt.character <- function(x, ...){
  # paths case
  if (length(x)>1)
    return(purrr::map(x, import_txt, ...))

  # single path
  x %>%
    utils::read.table(...) %>%
    Momocs2::coo_single()
}

#' @export
import_txt.mom_tbl <- function(x, from_col=path, to_col=coo, ...){
  from_col <- enquo(from_col)
  to_col   <- enquo(to_col)
  res <- x %>% dplyr::pull(!!from_col) %>% import_txt(...) %>% Momocs2::coo_list()
  dplyr::mutate(x, !!to_col := res)
}

