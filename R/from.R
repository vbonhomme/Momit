#' Convert a parsed filed to a mom_df that can be manipulated and exported
#'
#' @param x `character` typically what is returned by [momit]
#'
#' @return `mom_df` that can be manipulated and exported
#'
#' @name from
#' @rdname from
#' @family from functions
#'
#' @export
from_mom <- function(x){
  x %>%
    # here, foreign files can be manipulated
    # to look like a mom
    # for mom, they are just parsed
    parse_mom() %>% momify()
}


# (.mom) -> momit -> parse -> (mom) -> to
