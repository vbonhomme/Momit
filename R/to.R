#' Export morphometric data
#'
#' Only Momocs is supported so far.
#'
#' It's probably easy enough to build Momocs' objects (and others) from the `mom_df`
#' to not need many exporters. See vignette.
#'
#' @param x `mom_df` object, typically returned by one of [from]
#' @param coo `character` column` to use for `$coo` (default to `coo`)
#' @param fac `data.frame` to use for `$fac` (default to everything minus `coo`)
#' @param ... additional parameters to feed Momocs's builders see
#' [Momocs::Out()], [Momocs::Opn()], [Momocs::Ldk()]
#'
#' @aliases to_Momocs
#' @name to_Coo
#' @rdname to_Coo
#' @export
to_Out <- function(x, coo="coo", fac, ...){
  if (missing(fac))
    fac <- x[, grep(coo, colnames(x), invert=TRUE, value=TRUE)]
  Momocs::Out(x[, coo, drop=TRUE],
              fac=fac, ...)
}

#' @rdname to_Coo
#' @export
to_Opn <- function(x, coo="coo", fac, ...){
  if (missing(fac))
    fac <- x[, grep(coo, colnames(x), invert=TRUE, value=TRUE)]
  Momocs::Opn(x[, coo, drop=TRUE],
              fac=fac, ...)
}

#' @rdname to_Coo
#' @export
to_Ldk <- function(x, coo="coo", fac, ...){
  if (missing(fac))
    fac <- x[, grep(coo, colnames(x), invert=TRUE, value=TRUE)]
  Momocs::Ldk(x[, coo, drop=TRUE],
              fac=fac, ...)
}
