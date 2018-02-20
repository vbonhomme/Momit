#' Helper function to develop new from_* functions
#'
#' A dedicated vignette details their behaviour
#'
#' @param x `character` typically after a [harvest] or a [readLines]
#' @param pattern a [`regex`]
#' @param gather_by `numeric` how many elements per line
#' (most of the time this is the number of dimensions)
#' @param span `numeric` every `span` insert `this`
#' @param this `character` to insert every `span`.
#' If not provided, dummy names 'shp_NN' are inserted
#'
#' @return a `character`
#'
#' @name brush
#' @rdname brush
#' @export
brush_add_comment <- function(x, pattern){
  gsub(paste0("(", pattern, ")"), "#\\1", x)
}

#' @rdname brush
#' @export
brush_add_tildes <- function(x, pattern="^([[:alpha:]]+([[:alnum:]]|[[:punct:]])*)$"){
  gsub(pattern, "~\\1", x)
}

#' @rdname brush
#' @export
brush_remove_lines <- function(x, pattern){
  x[-grep(pattern, x)]
}

#' @rdname brush
#' @export
brush_remove_nts_dimensions <- function(x){
  grep("([[:digit:]]* ){3,}", x, value=TRUE, invert = TRUE)
}

#' @rdname brush
#' @export
brush_get_nts_nb_ind <- function(x){
  x %>%
    grep("([[:digit:]]* ){3,}", ., value=TRUE) %>%
    strsplit(" ") %>% `[[`(1) %>% `[`(2) %>%
    gsub("L", "", .) %>%
    as.numeric()
}

#' @rdname brush
#' @export
brush_get_nts_nb_coo <- function(x){
  x %>%
    grep("([[:digit:]]* ){3,}", ., value=TRUE) %>%
    strsplit(" ") %>% `[[`(1) %>% `[`(3) %>%
    gsub("L", "", .) %>%
    as.numeric()
}

#' @rdname brush
#' @export
brush_remove_multiple_spaces <- function(x){
  gsub(" {2, }", " ", x)
}

#' @rdname brush
#' @export
brush_remove_empty_lines <- function(x){
  x[nchar(x)>0]
}

#' @rdname brush
#' @export
brush_add_space_before <- function(x, pattern){
  gsub(pattern, paste0(" ", pattern), x)
}

#' @rdname brush
#' @export
brush_remove_leading_spaces <- function(x){
  gsub("^ +", "", x)
}

#' @rdname brush
#' @export
brush_remove_trailing_spaces <- function(x){
  gsub(" +$", "", x)
}

#' @rdname brush
#' @export
brush_reshape_lines <- function(x, pattern= " ", gather_by){
  x %>% lapply(function(i)
    # split and retain the first element (the only one)
    i %>% strsplit(pattern) %>% `[[`(1) %>%
      # build matrices, then collapse lines
      matrix(ncol=gather_by, byrow=T) %>%
      apply(1, paste, collapse= " ")) %>%
    # concatenate the entire list
    do.call("c", .)
}

#' @rdname brush
#' @export
brush_insert_every <- function(x, span, this){
  # deduce the number of "slices"
  z <- length(x)/span
  # create a vector and split the x to get a list
  f <- factor(rep(1:z, each=span))
  l <- x %>% split(f)
  # handles missing names
  if (missing(this))
    this <- paste0("~shp_", 1:z)
  # concatenate for each element, its name and its content
  lapply(1:z, function(i) c(this[i], l[[i]])) %>%
    # and concatenate the entire list
    do.call("c", .)
}

#
# readLines("tests/testthat/mosquito.nts") %>%
#   brush_remove_lines('"') %>%
#   brush_remove_lines("([[:alnum:]]+ ){3,}") %>%
#   brush_word_as_collated() %>%  parse_mom()
