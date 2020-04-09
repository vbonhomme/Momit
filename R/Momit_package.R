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
#' Thomas Cucchi, Carmelo Fruciano, Sabrina Renaud, Jim Rohlf,
#' Murat Maga, Chris Percival.
#'
#' Special thanks to Andrea Cardini for sharing very diverse datasets,
#' and to Allowen Evin for her patience and exotic data.
#'
#' @docType package
#' @name Momit
NULL

# prevents "no visible binding for global variable"
globalVariables(c(".", "partition"))
