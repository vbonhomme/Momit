#' Momit
#'
#' A minimalist file format for morphometrics data exchange and conversion.
#' Intended to be easy to read for humans, and easy to parse for
#' computers. Aimed at being a `pandoc` for morphometrics,
#' that is a swiss-army knife for converting files from/to the
#' different morphometric markup dialects.
#'
#' To cite Momit in publications: `citation("Momit")`.
#'
#' @section Cheers:
#' For their ideas, support and data samples,
#' I am very grateful to (in alphabetical order):
#' Andrea Cardini, Thomas Cucchi, Carmelo Fruciano, Jim Rohlf,
#' Murat Maga, Chris Percival.
#'
#' Special thanks to Allowen Evin
#' for her patience and exotic data.
#'
#' @docType package
#' @name Momit
NULL

# Import the pipe from dplyr
#' @importFrom dplyr "%>%"
#' @export
dplyr::"%>%"

# Import from dplyr
#' @importFrom dplyr bind_cols filter mutate slice transmute

# Import colors from crayon
#' @importFrom crayon green red silver

# Import print.tbl_df from tibble
# #' @importFrom tibble print.tbl_df

#' @importFrom tibble as_tibble

#' @importFrom StereoMorph readShapes

# prevents "no visible binding for global variable"
# http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
globalVariables(c(".", "what"))
