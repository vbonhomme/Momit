#' Helpers for example raw data
#'
#' A wrapper on top of [system.file] to ease Momit examples.
#'
#' @details
#' You do not need it for _your_ data.
#' Most of Momit examples use these functions.
#' In Momit examples (unit testing, R CMD CHECK etc.),
#' it helps find correct locations where raw example files bundled with the package are located.
#'
#' @param x `character` file(s) names from the example folder, only for `example_data`
#'
#' @return `character` the full path pointing to `Momit/extdata/`
#'
#' @references Adapted from [R-pkg](http://r-pkgs.had.co.nz/data.html#data-extdata) by Hadley Wickham.
#' @examples
#'  # List all example data
#'  Momit_data() %>% sniff()
#'
#' @name example_data
#' @export
Momit_data <- function(x=""){
  system.file("extdata", x, package = "Momit")
}

