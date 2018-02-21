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

#' @rdname from
#' @export
from_tps <- function(x, ...){
  if (!is.list(x)){
    x <- harvest(x, pattern="tps$", ...)
  }

  lapply(x, function(.x) .x %>%
           # move IMAGE on top and turn it into a collated
           brush_on_top("LM", "IMAGE") %>%
           brush_add_tildes("IMAGE=")  %>%
           # handle covariates (not partitions !)
           brush_gsub("([[:alnum:]]+)=([[:alnum:]]+)", "\\1 \\2") %>%
           brush_gsub("(LM).*", "\\1") %>%
           brush_gsub("(CURVES).*", "\\1") %>%
           brush_gsub("(POINTS).*", "\\1") %>%
           # in case we have eg curve \n points \n <some coords> points
           # remove curve and rename nested partitions
           brush_remove_empty_partition() %>%
           brush_rename_partition("~")
  ) %>%
    parse_mom() %>% momify()
}
