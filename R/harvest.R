#' Detect and read files
#'
#' A convenient wrapper around [list.files] and [readLines] that
#' also prepare files for the momification.
#'
#' @param pattern a [`regex`] to feed [list.files] (default to `` no subsetting)
#' @param where `character` a list of folders or files (default `getwd()`)
#' @param encoding one of `UTF-8` (default) or `latin1`
#' @param ... more arguments to feed [list.files]
#'
#' @note `from_*` importers harvest internally so most of the time you should not need
#' `harvest` alone. Internally, `list.files` is passed with `full.names=TRUE, recursive = TRUE`,
#' so that full names are returned and all subfolders (if any) will be scrapped.
#' If you do not want this behaviour, you can still harvest directly after the proper list (or single)
#' of paths, typically obtained with `list.files`.
#'
#' Also, some problems with encoding may occur, particularly with `latin1`.
#' `from_*` importers, when used directly, should message you with "see ?harvest".
#' In taht case, try changing the `encoding` parameter (see [readLines]).
#' @return a `list` of `character` lines
#' @export
harvest <- function(pattern="", where=getwd(), encoding="UTF-8", ...){
  # if folder(s) path(s) are provided,
  # list all paths to patterned files
  if (all(dir.exists(where))){
    x <- where %>%
      lapply(function(.) list.files(., full.names=TRUE, recursive = TRUE, ...) %>%
               grep(pattern=pattern, ., value=TRUE)) %>% # strange but otherwise doesnt work on folders
      do.call("c", .) %>%
      gsub("//", "/", .)
  }
  # if folders were provided or if a list of files
  # was directly provided, read them all
  if (all(sapply(x, file.exists))){
    x <- x %>%
      lapply(readLines, warn=FALSE, encoding=encoding) #%>%  # to prevent "incomplete final line" warning
      #`names<-`(x) # remove this in future for proper line indication
  }
  # check emptyness
  which_empty <- sapply(x, function(x) length(x)==0)
  # all of them case
  if (all(which_empty)){
    stop("\n",
         cli::symbol$cross,
         " all files were found empty")
  }
  # any of them case
  if (any(which_empty)){
    message("\n",
            cli::symbol$pointer,
            " files were found empty:\n",
            paste(names(x)[which_empty], collapse="\n"))
  }
  # return this beauty
  x
}
