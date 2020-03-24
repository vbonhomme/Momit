
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
#' chivas %>% to_yaml()
#'
#' @export
to_yaml <- function(x){
  x %>%
    yaml::as.yaml() %>%
    # add yaml class to benefit print.yaml
    `class<-`(c("yaml", class(.)))
}

#' @rdname to_yaml
#' @export
from_yaml <- function(x){
  x %>%
    yaml::yaml.load() %>%
    # turn into a tibble
    tibble::as_tibble() #%>%
    # no idea why rownmaes in as_tibble doesnt work
    # tibble::remove_rownames()
    # `attr<-`("row.names", NULL)
}


