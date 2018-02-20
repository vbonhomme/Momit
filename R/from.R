#' Convert a parsed filed to a mom_df that can be manipulated and exported
#'
#' @param x `character` typically what is returned by [harvest]; otherwise
#' bypassed with appropriate parameters. See the _Momit-usage_ vignette.
#' @param ... additional parameters to feed [harvest] (and [list.files] in the end)
#'
#' @return `mom_df` that can be manipulated and exported
#'
#' @name from
#' @rdname from
#' @family from functions
#'
#' @export
from_mom <- function(x, ...){
  if (!is.list(x)){
    x <- harvest(x, pattern="mom$", ...)
  }
  x %>%
    # here, foreign files can be manipulated
    # to look like a mom
    # for mom, they are just parsed
    parse_mom() %>% momify()
}
