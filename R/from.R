#' Convert a parsed filed to a mom_df that can be manipulated and exported
#'
#' @param x `character` typically what is returned by [harvest]; otherwise
#' bypassed with appropriate parameters. See the _Momit-usage_ vignette.
#' @param ... additional parameters to feed [harvest] (and [list.files] in the end)
#'
#' @return `mom_df` that can be manipulated and exported
#'
#' @name from
#' @rdname from
#' @family from functions
#'
#' @export
from_mom <- function(x, ...){
  if (!is.list(x)){
    x <- harvest(x, pattern="mom$", ...)
  }
  x %>%
    # here, foreign files can be manipulated
    # to look like a mom
    # for mom, they are just parsed
    parse_mom() %>% momify()
}

# tps* ------------------------------------------------
#' @rdname from
#' @export
from_tps <- function(x, ...){
  if (!is.list(x)){
    x <- harvest(x, pattern="tps$", ...)
  }
  # importer for 1
  from_tps1 <- function(.x) {
    .x %>%
      # move IMAGE on top and turn it into a collated
      brush_on_top("LM", "IMAGE") %>%
      brush_add_tildes("IMAGE=")  %>%
      # handle covariates (not partitions !)
      brush_gsub("([[:alnum:]]+)=([[:alnum:]]+)", "\\1 \\2") %>%
      brush_gsub("(LM).*", "\\1") %>%
      brush_gsub("(CURVES).*", "\\1") %>%
      brush_gsub("(POINTS).*", "\\1") %>%
      # in case we have eg curve \n points \n <some coords> points
      # remove curve and rename nested partitions
      brush_remove_empty_partition() %>%
      brush_rename_partition("~")
  }
  lapply(x, from_tps1) %>%
    parse_mom() %>% momify()
}

#' @rdname from
#' @export
from_nts <- function(x, ...){
  if (!is.list(x)){
    x <- harvest(x, pattern="nts$", ...)
  }
  # importer for 1
  nts1 <- function(.x){
    # first remove any comment
    x <- brush_remove_lines(.x, '"')
    # handles nts specifications
    # seems complicated but we need to deduce the dimensionnality
    # of the data since some people use nts for 3D data as well
    # extract the number of rows from the x1 y1 (z1) etc.
    nb_row <- brush_get_nts_nrow(x)
    # remove x1 y1 (z1) x2 y2 (z2) etc.
    x <- x %>% brush_remove_coordinates_pattern()
    # extract the number of coordinates
    nb_coo <- brush_get_nts_nb_coo(x)
    # and the number of individuals
    nb_ind <- brush_get_nts_nb_ind(x)
    # deduce the dimensionnality
    D <- nb_coo / nb_row
    # and get rid of this line
    x <- x %>% brush_remove_nts_dimensions()

    # possible weakness here
    # but sometimes we have names like 1_0_2384 ...
    nts_names <- "[[:alpha:]]+([[:alnum:]]|_)+"
    names_pos <- grep(nts_names , x)
    shp_names <- x[names_pos] %>%
      # prevent accidental multiple spaces
      gsub(" {2, }", " ", .) %>%
      # split and concatenate back
      strsplit(" ") %>%
      do.call("c", .) %>%
      paste0("~", .)
    # remove lines where names were previously matched/extracted
    x <- x[-names_pos]
    x %>%
      # reshape lines by their dimensionnality
      brush_reshape_lines(pattern = " ", gather_by = D) %>%
      # and insert names
      brush_insert_every(span = nb_coo/D, this = shp_names)
    # now ready to parse
  }
  lapply(x, nts1) %>%
    parse_mom() %>% momify()
}

# meshtools ------------------------------------------------

#' @rdname from
#' @export
from_stv <- function(x, ...){
  if (!is.list(x)){
    x <- harvest(x, pattern="stv$", ...)
  }
  # importer for 1
  from_stv1 <- function(.x) {
    .x %>%
      # remove Landmarks from concerned lines
      brush_gsub("Landmark[[:digit:]]+: ", "") %>%
      # remove dimension line
      brush_remove_lines("0 [[:digit:]]+")  %>%
      # reduce to 3 coordinates for concerned lines
      brush_shorten_coordinates(ncol=3)
  }
  lapply(x, from_stv1) %>%
    parse_mom() %>% momify()
}


#' @rdname from
#' @export
from_lmk <- function(x, ...){
  if (!is.list(x)){
    x <- harvest(x, pattern="lmk$", ...)
  }
  # importer for 1
  from_lmk1 <- function(.x) {
    .x %>%
      # remove Landmarks from concerned lines
      brush_gsub("Landmark[[:digit:]]+: ", "")
  }
  lapply(x, from_lmk1) %>%
    parse_mom() %>% momify()
}


# harvest("foreign/tpsDig_XYsusSEAsia.NTS")  %>% from_nts()

# harvest("foreign/tpsDig_guenons_online.nts")  %>%
# lapply(function(x) gsub("([[:digit:]]+_[[:digit:]])", "ind_\\1", x)) %>%
# from_nts()

# "foreign/meshtools_ZMK_TRF_01_34.lmk" %>% from_lmk

# "foreign/meshtools_TRF_01_34.stv" %>% from_stv
