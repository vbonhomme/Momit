#' Export morphometric data
#'
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
#' @name to
#' @rdname to
#' @export
to_Out <- function(x, coo="coo", fac, ...){
  if (missing(fac))
    fac <- x[, grep(coo, colnames(x), invert=TRUE, value=TRUE)]
  Momocs::Out(x[, coo, drop=TRUE],
              fac=fac, ...)
}

#' @rdname to
#' @export
to_Opn <- function(x, coo="coo", fac, ...){
  if (missing(fac))
    fac <- x[, grep(coo, colnames(x), invert=TRUE, value=TRUE)]
  Momocs::Opn(x[, coo, drop=TRUE],
              fac=fac, ...)
}

#' @rdname to
#' @export
to_Ldk <- function(x, coo="coo", fac, ...){
  if (missing(fac))
    fac <- x[, grep(coo, colnames(x), invert=TRUE, value=TRUE)]
  Momocs::Ldk(x[, coo, drop=TRUE],
              fac=fac, ...)
}


# domestic classes --------

#' @rdname to
#' @export
to_mom <- function(x, ...){
  UseMethod("to_mom")
}

#' @rdname to
#' @export
to_mom.matrix <- function(x, ...){
  # to prevent engineer notation
  op <- options(scipen=999)
  on.exit(options(op))
  # return line by line
  apply(x, 1, paste0, collapse=" ", ...)
}

#' @rdname to
#' @export
to_mom.array <- function(x, ...){
  # a2l
  l <- lapply(1:dim(x)[3], function(i) x[,,i])
  # if names, use them
  if (!is.null(names(x)[3]))
    names(l) <- names(x)[3]
  # to to_mom.list
  l %>% to_mom
}

#' @rdname to
#' @export
to_mom.list <- function(x, ...){
  l <- lapply(x, to_mom, ...)
  # if no names, dummy names
  if (is.null(names(l)))
    n <- paste0("~shp", seq_along(l))
  # prepend each component with its names
  l <- mapply(c, n, l, SIMPLIFY = F)
  # unlist and remove names
  l %>% unlist %>% `names<-`(NULL)
}

#' @rdname to
#' @export
to_mom.data.frame <- function(x, ...){
  # if no rownames dummy ones
  # since (no NULL on data.frame but "1", "2", etc.)
  if (identical(rownames(x), as.character(1:nrow(x)))){
    n <- paste0("~shp", 1:nrow(x))
  } else {
    # otherwise use them
    n <- rownames(x)
  }
  # each row is momified
  l <- lapply(1:nrow(x), function(i) .df_2_str(x[i, ]))
  names(l) <- n
  # prepend each component with its names
  l <- mapply(c, n, l, SIMPLIFY = F)
  # unlist and remove names
  l %>% unlist %>% `names<-`(NULL)
}

#' @rdname to
#' @export
to_mom.Coo <- function(x, ...){
  # coo
  coo_l <- lapply(x$coo, to_mom, ...)
  # fac
  fac_l <- lapply(1:nrow(x$fac), function(i) .df_2_str(x$fac[i, ]))
  # if no names, dummy ones
  if (is.null(names(x))){
    n <- paste0("~shp", seq_along(coo_l))
  } else {
    n <- paste0("~", names(x))
  }
  # concatenate element-wise
  mapply(c, n, coo_l, fac_l, SIMPLIFY = FALSE) %>%
  # remove names and unlist and return this beauty
    unlist %>% `names<-`(NULL)
}



