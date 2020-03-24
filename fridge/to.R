#' Translate mom to foreign file format
#'
#' Given a `mom_df`, translate it into a foreign file format
#' that can be written later on with [write].
#'
#' @param x `mom_df`` to convert
#' @return `character` lines that can be written with [write]
#'
#' @family from functions
#' @name to
#' @rdname to
#'
#' @export
to_mom <- function(x){
  lapply(1:nrow(x), function(i) x[i, ] %>% .mom_df_2_mom) %>%
    `names<-`(x$name)
}

#' @rdname to
#' @export
to_Out <- function(x){
  Momocs::Out(x$coo, fac=x[, -(1:2)])
}

#' @rdname to
#' @export
to_Opn <- function(x){
  Momocs::Opn(x$coo, fac=x[, -(1:2)])
}

#' @rdname to
#' @export
to_Ldk <- function(x){
  Momocs::Ldk(x$coo, fac=x[, -(1:2)])
}
