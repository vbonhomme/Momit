#' Helpers for example raw data
#'
#' A wrapper on top of [system.file] to ease Momit examples. See examples below.
#'
#' Most of Momit __examples__ use these functions. This is not intended for real world, that is for __your__ files.
#' Here, it helps find correct locations where raw example files bundled with the package are located.
#' Providing that your working directory contains the file(s) you are
#' interested in, everything should work fine, just like in the vignette.
#'
#' @param x `character` file(s) names from the example folder, only for `example_data`
#'
#' @return `character` the full path pointing to the file (for `example_data`)
#' or to the containing folder (for `example_dir`)
#' where examples data have been copied on your computer.
#'
#' @references Adapted from [R-pkg](http://r-pkgs.had.co.nz/data.html#data-extdata) by Hadley Wickham.
#' @examples
#'  # Example folder path
#'  example_dir()
#'
#'  # List all example data
#'  example_dir() %>% sniff()
#'
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


