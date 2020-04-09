# messy paths ---------------------------------------------

# messy_paths <- c("Big_one/folder-foo/file_n.jpg",
#                  "folder-foo/File n.jpg",
#                  "folder-foo2/file   5.jpg",
#                  "file.txt",
#                  "file.jpeg",
#                  "file.png",
#                  "file.mom")
# usethis::use_data(messy_paths, overwrite = TRUE)

#' Some messy paths
#'
#' A nice vector of paths with many problems
#'
"messy_paths"

# chivas --------------------------------------------------

# chivas <-Momocs2::bot %>% dplyr::mutate(coo=Momocs2::new_coo_list(coo)) %>% dplyr::slice(24) %>%
# Momocs2::coo_sample(60) %>% dplyr::mutate(n=pi, foo=NA)
# usethis::use_data(chivas, overwrite=TRUE)

#' Chivas bottle outline
#'
#' A single outline
#'
#' @format A single-row [Momocs2::mom] with:
#' \describe{
#'   \item{coo}{a list of matrices}
#'   \item{type}{of bottle}
#'   \item{fake}{factor}
#'   \item{n}{some numeric}
#'   \item{foo}{NA}
#' }
"chivas"
