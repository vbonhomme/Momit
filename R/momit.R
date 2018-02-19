#' Detect and read files
#'
#' A convenient wrapper around [list.files] and [readLines] that
#' also prepare files for the momification.
#'
#' @param x `character` a list of folders or files (default `getwd()`)
#' @param pattern a [`regex`] to feed [list.files] (default to `\\.mom$`)
#' @param ... more arguments to feed [list.files]
#'
#' @return `character` lines that can be momified with [parse_mom]
#'
#' @export
momit <- function(x=getwd(), pattern="mom$", ...){
  # if folder(s) path(s) are provided,
  # list all paths to patterned files
  if (all(dir.exists(x))){
    x <- x %>%
      lapply(function(.) list.files(., full.names=TRUE, pattern=pattern, ...)) %>%
      do.call("c", .) %>%
      gsub("//", "/", .)
  }
  # if folders were provided or if a list of files
  # was directly provided, read them all
  if (all(sapply(x, file.exists))){
    x %>%
      lapply(readLines) %>%
      lapply(.prune)
  }
}

