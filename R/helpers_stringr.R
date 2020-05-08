# path_fix ------------------------------------------------
# str_helpers
paths <- c("Big_one/folder-foo/file_n.jpg",
           "folder-foo/File n.jpg",
           "folder-foo2/file   5.jpg",
           "file.txt",
           "file.jpeg",
           "file.png",
           "file.mom")

#' String helpers
#'
#' Simple yet useful wrappers for working on strings.
#' Most of them are built on top of existing [stringr] functions ;
#' not all return strings.
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
#' @name str_helpers
#' @family path
#'
#' @examples
#' messy_paths
#' messy_paths %>% str_underscorise()
NULL

#' @describeIn  str_helpers remove empty lines, turn tabs into spaces and remove multiple spaces
#' @export
str_remove_useless_spaces <- function(x){
  # remove empty lines
  x <- x[stringr::str_length(x) > 0]

  x %>%
    # turn tab(s) to spaces
    stringr::str_replace("(\t)+", " ") %>%
    # remove trailing, trimming and multiple spaces
    stringr::str_squish()
}

#' @describeIn  str_helpers turn dashes and spaces into underscores
#' @export
str_underscorise <- function(x, pattern="-+|[[:space:]]+"){
  stringr::str_replace_all(x, pattern=pattern, "_")
}

#' @describeIn  str_helpers trim extension
#' @export
str_trim_ext <- function(x, pattern="\\.[[:alnum:]]+$"){
  stringr::str_remove(x, pattern=pattern)
}

#' @describeIn  str_helpers trim dirpath
#' @export
str_trim_dirpath <- function(x, pattern="^.*/"){
  stringr::str_remove(x, pattern=pattern)
}

# turns a vector of characters,
# with some elements beginning with a pattern,
# split it into a list
# c("a", "b", "ab", "ac", "b") %>% .split_where_pattern("a")
.splitting_vector <- function(x, pattern){
  # detect pattern and prepare single partition
  ids <- stringr::str_which(x, pattern)

  # early return if nothing detected
  if (length(ids)==0)
    return(rep(1, length(x)))

  # otherwise prepare a splitting vector
  f   <- rep(NA_integer_, length(x))

  # other complete the f
  f[ids] <- seq_len(length(ids))

  y <- !is.na(f)
  cumsum(y)+1
}

#' @describeIn str_helpers given a string, split on pattern
str_split_on <- function(x, pattern){
  split(x, .splitting_vector(x, pattern))
}

#' @describeIn  str_helpers turn coordinates as vector of strings into a matrix
#' @export
str_to_mat <- function(x){
  x %>%
    stringr::str_split(" +") %>%
    purrr::map(as.numeric) %>%
    do.call("rbind", .)
}

#' @describeIn  str_helpers drop elements with pattern
#' @export
str_drop <- function(x, pattern){
  x[stringr::str_detect(x, pattern, negate = TRUE)]
}

#' @describeIn  str_helpers turn a string with pattern on a named list
#' @export
str_to_named_list <- function(x, pattern){
  # split on pattern
  z <- x %>% stringr::str_split("=")
  # take second element (was on the right hand side of pattern)
  # also remove leading, trailing and multiple spaces
  res <- purrr::map(z, 2) %>%
    purrr::map(stringr::str_squish)
  # take first element (was on the left hand side of pattern)
  # also lowercase
  names(res) <- purrr::map_chr(z, 1) %>%
    stringr::str_to_lower()
  # return this beauty
  res
}



# path_extract --------------------------------------------
#' Extract information from paths
#'
#' Only reexport from [fs]
#'
#' @param x `character` of one or more paths
#'
#' @return [character]
#' @seealso
#' To _fix_ information from paths see [str_helpers]
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

# list_files ----------------------------------------------
#' List files in path
#'
#' A wrapper on top of [list.files] with `recursive` and `full.names` set to `TRUE`
#'
#' @param x path
#' @param ... other arguments passed to [list.files]
#' @return list of files
#'
#' @export
#' @name path_helpers
#' @examples
#' Momit_data("acer") %>% list.files()
#' Momit_data("acer") %>% list_files()
list_files <- function(x, ...){
    list.files(x, full.names=TRUE, recursive=TRUE, ...)
  }

#' @describeIn path_helpers test if a path has an image extension
#' @export
is_imagepath <- function(x){
  extract_ext(x) %in% image_ext
}

# Regex helpers
image_ext <- c("jpg", "png", "tiff")



