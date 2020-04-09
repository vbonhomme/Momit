# path_fix ------------------------------------------------
# str_helpers
paths <- c("Big_one/folder-foo/file_n.jpg",
           "folder-foo/File n.jpg",
           "folder-foo2/file   5.jpg",
           "file.txt",
           "file.jpeg",
           "file.png",
           "file.mom")

#' Fix messy paths
#'
#' Useful (and simple) wrappers around [stringr] functions.
#'
#' @param x [character]
#' @param pattern [regex] (a regular expression)
#'
#' @return [character]
#' @seealso
#' To _extract_ information from paths see [extract_path]
#'
#' To _fix_ information from paths, besides these ones,
#'  you have plenty friends in [stringr], and particularly:
#'
#' * [stringr::str_to_lower](str_to_lower): lowercase them all
#' * [stringr::str_trim]: remove whitespace from start and end of string
#'
#'
#' @name fix_path
#' @family path
#'
#' @examples
#' messy_paths
#' messy_paths %>% str_underscorise()
NULL

#' @describeIn  fix_path turn dashes and spaces into underscores
#' @export
str_underscorise <- function(x, pattern="-+|[[:space:]]+"){
  stringr::str_replace_all(x, pattern=pattern, "_")
}

#' @describeIn  fix_path trim extension
#' @export
str_trim_ext <- function(x, pattern="\\.[[:alnum:]]+$"){
  stringr::str_remove(x, pattern=pattern)
}

#' @describeIn  fix_path trim dirpath
#' @export
str_trim_dirpath <- function(x, pattern="^.*/"){
  stringr::str_remove(x, pattern=pattern)
}

# expect_false(paths %>% str_underscorise() %>% str_detect("-") %>% any())

# path_extract --------------------------------------------
#' Extract information from paths
#'
#' Only reexport from [fs]
#'
#' @param x `character` of one or more paths
#'
#' @return [character]
#' @seealso
#' To _fix_ information from paths see [fix_path]
#'
#'
#' @name extract_path
#' @family path
#'
#' @examples
#' messy_paths
#' messy_paths %>% extract_dir()
#' messy_paths %>% extract_file()
#' messy_paths %>% extract_ext()
#'
#' # all of this in a tibble
#' tibble::tibble(paths = messy_paths,
#'               dir    = extract_dir(paths),
#'               file   = extract_file(paths),
#'               ext    = extract_ext(paths))
NULL

#' @describeIn  extract_path extract directory path
#' @export
extract_dir <- function(x){
  fs::path_dir(x)
}

#' @describeIn  extract_path extract filename
#' @export
extract_ext <- function(x){
  fs::path_ext(x)
}

#' @describeIn  extract_path extract file extension
#' @export
extract_file <- function(x){
  fs::path_file(x)
}

# Regex helpers
image_ext <- c("jpg", "png", "tiff")



