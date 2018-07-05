#' Parse morphometric file data to a mom_df
#'
#' Parse, eg polish and analyse the syntax of, morphometric file data.
#' Most of the time you should prefer [from]
#'
#' @details The `parse_*1` and `parse_*` dichotomy is not redundant:
#' depending on formatting and/or the possible collated nature of the data,
#' some polishing steps must be done before or after these individual files are split.
#'
#' @param x `vector` or `list` (for `parse_*1` and `parse_*`) of lines to parse
#' @return `mom_list` or `mom_df` (for `parse_*1` and `parse_*`)
#'
#' @name parse
#' @rdname parse

# mom -----------------------------------------------------
#' @rdname parse
#' @export
parse_mom1 <- function(x){
  x %>%
    .remove_non_valid() %>%
    # extract components
    sift()
}

#' @rdname parse
#' @export
parse_mom <- function(x){
  # prune, split and lapply parse1
  x %>%
    # in case we have a list (eg several files at the beginning)
    do.call("c", .) %>%
    .prune %>%
    .split_on("~") %>%
    lapply(parse_mom1) %>%
    momify()
}


# tps -----------
#' @rdname parse
#' @export
parse_tps1 <- function(x){
  x %>%
    gsub("^IMAGE=(.*)$", "~\\1", .) %>%
    .on_top("~") %>%
    # turn 'LM=.*' into 'LM' (proper mom partition names)
    gsub("(LM).*", "\\1", .) %>%
    # turn 'POINTS=.*' into POINTS (same)
    gsub("(POINTS).*", "\\1", .) %>%
    # remove lines containing 'CURVE'
    grep("CURVE", ., invert=TRUE, value=TRUE) %>%
    # replace '='  with ' ' (for proper mom covariates)
    gsub("=", " ", .) %>%
    # extract components
    sift()
}

#' @rdname parse
#' @export
parse_tps <- function(x){
  # prune, split and lapply parse1
  x %>%
    # in case we have a list (eg several files at the beginning)
    do.call("c", .) %>%
    .prune %>%
    .split_on("LM") %>%
    lapply(parse_tps1) %>%
    momify()
}

# from_tps("tests/testthat/tpsDig_allowen1.tps")
# from_tps("tests/testthat/tpsDig_allowen2.tps")
# from_tps("tests/testthat/tpsDig_allowen3.tps")
# from_tps("tests/testthat/tpsDig_allowen4.tps")
# from_tps("tests/testthat/tpsDig_30LMs_TOTAL.tps", encoding="latin1")

# # stv -----
# parse_stv1 <- function(x){
#   x %>%
#     .prune %>%
#     # remove dimension line
#     grep("0 [[:digit:]]+", ., invert=TRUE, value=TRUE)  %>%
#     # remove Landmarks from concerned lines
#     gsub("Landmark[[:digit:]]+: ", "", .) %>%
#     # reduce to 3 coordinates for concerned lines
#     .shorten_coordinates(ncol=3) %>%
#     # extract components
#     sift()
# }
#
# parse_stv <- function(x){
#   if (!is.list(x))
#     x <- list(x)
#   x %>%
#     lapply(parse_stv1) %>%
#     momify()
# }
#
# # readLines("tests/testthat/meshtools_TRF_01_34.stv") %>% parse_stv()
#
#
# # lmk --------
#
# parse_lmk1 <- function(x){
#   x %>%
#     # remove Landmarks from concerned lines
#     brush_gsub("Landmark[[:digit:]]+: ", "") %>%
#     sift()
# }
#
# parse_lmk <- function(x){
#   if (!is.list(x))
#     x <- list(x)
#   x %>%
#     lapply(parse_lmk1) %>%
#     momify()
# }

# readLines("tests/testthat/meshtools_ZMK_TRF_01_34.lmk") -> x
# harvest("tests/testthat/meshtools_ZMK_TRF_01_34.lmk") %>% parse_lmk
# harvest("tests/testthat/meshtools_ZMK_TRF_01_34.lmk") %>% parse_lmk

# nts ----------
#
# readLines("tests/testthat/mosquito.nts") -> x
#
# # sounds like a plan
# parse_nts1 <- parse_mom1
#
# parse_nts <- function(x){
#   x %>%
#     .prune %>%
#     # remove comment lines
#     grep('"', ., invert=TRUE, value=TRUE) %>%
#     # remove dimensions lines
#     grep("^1 [[:digit:]]+", ., invert=TRUE, value=TRUE) %>%
#     # add tildes on single names
#     gsub("^([[:alpha:]]+([[:alnum:]]|[[:punct:]])*)$", "~\\1", .) %>%
#     .split_on("~") %>%
#     lapply(parse_nts1) %>%
#     momify()
# }
#
# readLines("tests/testthat/tpsDig_ontogeny9L.nts") %>% parse_nts()
# readLines("tests/testthat/tpsDig_ontogeny9L.nts")->x
