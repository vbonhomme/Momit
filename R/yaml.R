
# utils ----
#' @export
print.yaml <- function(x, ...){
  cat(x)
}

# yaml ----------------------------------------------------
#' yaml wrappers
#'
#' Around `pkg::yaml` base functions
#'
#' @param x any object
#'
#' @examples
#' (chivas$coo[[1]] %>% export_yaml() -> x)
#' x %>% import_yaml()
#' @export
export_yaml <- function(x){
  x %>%
    Momocs2::coo_single() %>%
    yaml::as.yaml() %>%
    # add yaml class to benefit print.yaml
    `class<-`(c("yaml", class(.)))
}

#' @rdname export_yaml
#' @export
import_yaml <- function(x){
  x %>%
    yaml::yaml.load() %>%
    # turn into a tibble
    Momocs2::coo_single()
    # no idea why rownmaes in as_tibble doesnt work
    # tibble::remove_rownames()
    # `attr<-`("row.names", NULL)
}


