# chivas <- Momocs2::bot2 %>% dplyr::slice(24) %>% dplyr::mutate(size=-123.45, missing=NA)
# usethis::use_data(chivas)

#' A one row coo_tbl from Momocs
#'
#' Obtained with:
#'  ```
#'    bot2 %>%
#'      dplyr::select(-fake) %>%
#'      dplyr::slice(24) %>%
#'      dplyr::mutate(size=-123.45, missing=NA)
#' ```
#'
#' @format A coo_tbl with
#' \describe{
#'   \item{coo}{a list of matrices}
#'   \item{type}{of bottle}
#'   \item{missing}{NA}
#'   \item{size}{a numeric}
#' }
#' @source Momocs
"chivas"
