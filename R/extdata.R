#' Helpers for example raw data
#'
#' A wrapper on top of [system.file] to ease Momit examples. See examples below.
#'
#' Most of Momit examples use this to cope with different file locations between
#' your machine and mine. Providing that your working directory contains the file(s) you are
#' interested in, or if you provide argument `where` to [from]/[harvest] everything should work fine,
#' just like in the vignette.
#'
#' @param x `character` file(s) names from the example folder
#'
#' @return `example_data` will return
#' the full path where passed files are; `example_data` will return the containing folder
#' where examples data have been copied on your computer.
#'
#' @references Adapted from [R-pkg](http://r-pkgs.had.co.nz/data.html#data-extdata) by Hadley Wickham.
#' @examples
#'  # Example folder path
#'  example_dir()
#'
#'  # List all example data
#'  list.files(example_dir(), recursive=TRUE, full.names=TRUE)
#'
#'  # path to one of them
#'  example_data("bot_lite.mom")
#'
#'  # parse it
#'  example_data("bot_lite.mom") %>% harvest() %>% parse_mom()
#'
#' # or shorter:
#' from_mom(example_data("bot_lite.mom"))
#' @name example_data
#' @export
example_data <- function(x){
  system.file("extdata", x, package = "Momit")
}

#' @name example_data
#' @export
example_dir <- function(){
  system.file("extdata",  package = "Momit")
}

# example_data_copy <- function(where=getwd()){
#   fs::dir_copy(example_dir(), where)
# }


