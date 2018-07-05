# .mom ----------------------------------------------------
#' Import morphometric files data
#'
#' @details `from_*` are nothing but shortcuts on top of [harvest] and [parse].
#' Type for example `from_mom` (no brackets) to see it.
#' @section Currently supported formats:
#'
#'  * `.mom` with `from_mom`
#'  * `.tps` with `from_tps`
#'
#'
#'
#' @param pattern a regular expression (see [regex] and examples below) to subset the list of
#' harvested files. Defaults to `filetype$` for each `from_filetype` (eg `tps$` for `from_tps`), etc.
#' @param where either path(s) or a `list` of `character`s typically obtained with `harvest` or the raw `readLines`.
#' By default, `setwd()` so that all files somewhere in your current working directory will be
#' `harvest`ed and imported.
#' @param encoding see [harvest]
#' @param ... additional parameters to feed [harvest] (and [list.files] in the end)
#'
#' @return `mom_df` that can be manipulated and exported
#' @note If you are not yet familiar with regular expression, go learn them, they are
#' among the most profitable investment you can make when talking to a computer.
#' @name from
#' @rdname from
#' @family from functions
#' @examples
#' # See ?example_data (you should not need the `where` argument below)
#'
#' # grab all files containing `mom` in the directory
#' # of course we have some NAs below since we are merging "different" .mom
#' # with different cov, etc. components
#' from_mom("mom", example_dir())
#'
#' # grab all files within the 'bot_lite'  folder
#' # this one works like a charm
#' from_mom("bot_lite/", example_dir())
#'
#'
#' # same for tps, all files that begin with tpsDig_allo
#' from_tps("tpsDig_allo", example_dir())
#'
#' # more restrictively
#' from_tps("tpsDig_allowen1", example_dir())
#'
#' # or if you prefer a real path rather than a matching pattern
#' # (again, you should not need 'example_data()'), see ?example_data
#' harvest(example_data("tpsDig_allowen1.tps")) %>%
#'   parse_tps()
#' @export
from_mom <- function(pattern="mom$", where=getwd(), encoding="unknown", ...){
  if (!is.list(where))
    x <- harvest(pattern=pattern, where=where, encoding=encoding, ...)
  else
    x <- where
  x %>% parse_mom()
}

# tps* ------------------------------------------------
#' @rdname from
#' @export
from_tps <- function(pattern="tps$", where=getwd(), encoding="unknown", ...){
  if (!is.list(where))
    x <- harvest(pattern=pattern, where=where, encoding=encoding, ...)
  else
    x <- where
  x %>% parse_tps
}

#' #' @rdname from
#' #' @export
#' from_nts <- function(x=getwd(), pattern="nts$", ...){
#'   if (!is.list(x)){
#'     x <- harvest(x, pattern=pattern, ...)
#'   }
#'   # importer for 1
#'   nts1 <- function(.x){
#'     # first remove any comment
#'     x <- brush_remove_lines(.x, '"')
#'     # handles nts specifications
#'     # seems complicated but we need to deduce the dimensionnality
#'     # of the data since some people use nts for 3D data as well
#'     # extract the number of rows from the x1 y1 (z1) etc.
#'     nb_row <- brush_get_nts_nrow(x)
#'     # remove x1 y1 (z1) x2 y2 (z2) etc.
#'     x <- x %>% brush_remove_coordinates_pattern()
#'     # extract the number of coordinates
#'     nb_coo <- brush_get_nts_nb_coo(x)
#'     # and the number of individuals
#'     nb_ind <- brush_get_nts_nb_ind(x)
#'     # deduce the dimensionnality
#'     D <- nb_coo / nb_row
#'     # and get rid of this line
#'     x <- x %>% brush_remove_nts_dimensions()
#'
#'     # possible weakness here
#'     # but sometimes we have names like 1_0_2384 ...
#'     nts_names <- "[[:alpha:]]+([[:alnum:]]|_)+"
#'     names_pos <- grep(nts_names , x)
#'     shp_names <- x[names_pos] %>%
#'       # prevent accidental multiple spaces
#'       gsub(" {2, }", " ", .) %>%
#'       # split and concatenate back
#'       strsplit(" ") %>%
#'       do.call("c", .) %>%
#'       paste0("~", .)
#'     # remove lines where names were previously matched/extracted
#'     x <- x[-names_pos]
#'     x %>%
#'       # reshape lines by their dimensionnality
#'       brush_reshape_lines(pattern = " ", gather_by = D) %>%
#'       # and insert names
#'       brush_insert_every(span = nb_coo/D, this = shp_names)
#'     # now ready to parse
#'   }
#'   # import and parse them all
#'   lapply(x, nts1) %>%
#'     parse_mom() %>% momify()
#' }
#'
#' # meshtools ------------------------------------------------
#'
#' #' @rdname from
#' #' @export
#' from_stv <- function(x, ...){
#'   if (!is.list(x)){
#'     x <- harvest(x, pattern="stv$", ...)
#'   }
#'   # importer for 1
#'   from_stv1 <- function(.x) {
#'     .x %>%
#'       # remove Landmarks from concerned lines
#'       brush_gsub("Landmark[[:digit:]]+: ", "") %>%
#'       # remove dimension line
#'       brush_remove_lines("0 [[:digit:]]+")  %>%
#'       # reduce to 3 coordinates for concerned lines
#'       brush_shorten_coordinates(ncol=3)
#'   }
#'   # import and parse them all
#'   lapply(x, from_stv1) %>%
#'     parse_mom() %>% momify()
#' }
#'
#'
#' #' @rdname from
#' #' @export
#' from_lmk <- function(x, ...){
#'   if (!is.list(x)){
#'     x <- harvest(x, pattern="lmk$", ...)
#'   }
#'   # importer for 1
#'   from_lmk1 <- function(.x) {
#'     .x %>%
#'       # remove Landmarks from concerned lines
#'       brush_gsub("Landmark[[:digit:]]+: ", "")
#'   }
#'   # import and parse them all
#'   lapply(x, from_lmk1) %>%
#'     parse_mom() %>% momify()
#' }

# #' @rdname from
# #' @export
# from_StereoMorph <- function(x, ...){
#   if (!is.list(x)){
#     x <- harvest(x, ...)
#   }
#   # importer for 1
#   # what about Filter(Negate(is.null), x)
#   StereoMorph1 <- function(.x){
#     # rewrite file in a tmp file
#     tmp <- paste0(tempfile(), "tmp.txt")
#     writeLines(.x, tmp)
#     # on exit, remove it
#     on.exit(silent <- file.remove(tmp))
#     x <- StereoMorph::readShapes(tmp)
#
#     # to host mom-like lines
#     res <- character()
#
#     # handles image.id if any
#     if (!is.null(x$image.id)){
#       res <- append(res, paste0("~", x$image.id))
#     } else {
#       res <- append(res, paste0("~", "shp"))
#     }
#
#     # handles scaling if any
#     if (!is.null(x$scaling))
#       res <- append(res, paste("scaling", x$scaling))
#
#     # handles landmarks if any - if not scaled try pixel
#     if (!is.null(x$landmarks.scaled)) {
#       x$landmarks.scaled %>% `rownames<-`(NULL) %>%
#         .mtx_2_str() %>% c("landmarks", .) %>% append(res, .) -> res
#     } else {
#       if (!is.null(x$landmarks.pixel)) {
#         x$landmarks.pixel %>% `rownames<-`(NULL) %>%
#           .mtx_2_str() %>% c("landmarks", .) %>% append(res, .) -> res
#       }
#     }
#
#     # handles curves if any - if not scaled try pixel
#     if (!is.null(x$curves.scaled)){
#       curves_names <- paste0("curve_", names(x$curves.scaled))
#       curves_str <- x$curves.scaled %>% lapply(.mtx_2_str)
#       lapply(seq_along(curves_str),
#              function(i) c(curves_names[i], curves_str[[i]])) %>%
#         do.call("c", .) %>% append(res, .) -> res
#     } else {
#       if (!is.null(x$curves.pixel)){
#         curves_names <- paste0("curve_", names(x$curves.pixel))
#         curves_str <- x$curves.pixel %>% lapply(.mtx_2_str)
#         lapply(seq_along(curves_str),
#                function(i) c(curves_names[i], curves_str[[i]])) %>%
#           do.call("c", .) %>% append(res, .) -> res
#       }
#     }
#     res
#   }
#
#   # import and parse them all
#   lapply(x, StereoMorph1) %>%
#     parse_mom() %>% momify()
# }


# #' @rdname from
# #' @export
# from_Optimas <- function(x, ...){
#   if (!is.list(x)){
#     x <- harvest(x, prune=FALSE, ...)
#   }
#
#   Optimas1 <- function(.x){
#     # add (dummy) names where empty lines are
#     x <- brush_add_names_empty_lines(.x)
#     # x <- .x
#     # find tab separated words
#     grep("([[:alpha:]])+", x, value=TRUE) %>%
#       # remove leading/trailing space characters
#       gsub("(^[[:space:]]*)|([[:space:]]*$)", "", .) %>%
#       # replace space characters (tabs here) with single space
#       brush_replace_space_characters_with_space() %>%
#       # split on single space
#       strsplit(" ") %>%
#       # back to vector
#       unlist() -> cov_names
#
#     # handles Optimas special cases
#     SampledPoints_pos <- grep("ArSampledPoints", cov_names)
#     if (length(SampledPoints_pos)>0)
#       cov_names <- c(cov_names[-SampledPoints_pos], paste0("ArSampledPoints", "_", c("x", "y")))
#
#     # deduce the number of cov
#     cov_nb <- length(cov_names)
#     # remove header line
#     x <- x[-grep("([[:alpha:]])+", x)]
#     # cov lines are those with the same number of covs than words in cov_names
#     # not perfect but we trimmed tabs before
#     cov_lines <- strsplit(x, " ") %>% sapply(length) %>% `==`(length(cov_names)) %>% which()
#
#     # get rid of multiple tabs and leading/trailing spaces
#     x <- brush_gsub(x, "\t", " ") %>%
#       brush_remove_multiple_spaces() %>%
#       brush_remove_leading_spaces() %>%
#       brush_remove_trailing_spaces()
#
#     # add mom-like cov_names to cov_values
#     cov_ready <- strsplit(x[cov_lines], " ") %>%
#       lapply(function(.x) paste(cov_names, .x))
#
#     # add them, replace cov lines and return
#     brush_insert_this_at(x, cov_ready, cov_lines)
#   }
#
#   lapply(x, Optimas1) %>%
#     parse_mom %>% momify
# }

#'
#' #' @rdname from
#' #' @export
#' from_PAST <- function(x, ...){
#'   if (!is.list(x)){
#'     x <- harvest(x, ...)
#'   }
#'
#'   PAST1 <- function(.x){
#'     x  <- .x %>%
#'       brush_remove_lines("RawCoord") %>%
#'       brush_replace_space_characters_with_space()
#'
#'     # explode, grab name and coordinates,
#'     # reformat them to mom-like and concatenate back
#'     past_single_conf <- function(x){
#'       x <- x %>% strsplit(" ") %>% unlist
#'       c(paste0("~", x[1]),
#'         x[-1] %>% paste(collapse=" ") %>% brush_reshape_lines(gather_by=2))
#'     }
#'     x %>%
#'       # apply to all lines and concatenate back
#'       lapply(past_single_conf) %>%
#'       do.call("c", .)
#'   }
#'
#'
#'   lapply(x, PAST1) %>%
#'     parse_mom %>% momify
#' }
