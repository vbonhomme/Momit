#' Turns mom_list(s) into mom_df
#'
#' Turns `mom_list` returned by [parse]rs into `data_frame`s, much easier to handle.
#' Most of the time you should not need it directly expect if you write your own [parse] function.
#'
#' @param x `character`or `list` of `character` for `momify1` and `momify`, respectively.
#' @return `mom_df`
#'
#'
#' @examples
#' \dontrun{
#' # a glimpse at what occurs behind the curtain
#' x <- harvest(example_data("bot_lite.mom")) # harvest bot_lite.mom
#' x[[1]] %>%
#'  Momit:::.split_on("~") %>% # here we do not have so much polish as we have a mom
#'  lapply(parse_mom1) -> moms
#'  # we now have a list of 'mom_list's
#'  class(moms[[1]])
#'  # that can be momify-ed to mom_df
#'  momify1(moms[[1]])
#'  # all of them now
#'  lapply(moms, momify1)
#'  # now we just have to rbind them all
#'  do.call("rbind", lapply(moms, momify1)) # we now have the full mom_df
#'  #
#'  dplyr::bind_rows(lapply(moms, momify1)) # better since it allows column heterogeneity
#'  # that is actually what momify (no 1) does with more checks
#'  momify(moms)
#'
#'  # and from_mom wraps all of these steps (with additional benefits in terms of checking)
#'  m1 <- harvest(example_data("bot_lite.mom")) %>%
#'          `[[`(1) %>%
#'          Momit:::.split_on("~") %>% # this one is not exported (yet usable)
#'          lapply(parse_mom1) %>%
#'          lapply(momify1) %>%
#'          dplyr::bind_rows()
#'  m2 <- from_mom(example_data("bot_lite.mom"))
#'  identical(m1, m2)
#'  }
#' @name momify
#' @rdname momify
#' @export
momify1 <- function(x){

  # the best structure I have found
  # to deal with all or some empty components,
  # retain names and end with a single-row data_frame

  # name ---
  if (is.na(x$name))
    name <- NULL
  else
    name <- dplyr::data_frame(name=x$name)

  # cov ---
  if (length(x$cov)==1 && is.na(x$cov))
    cov <- NULL
  else
    cov <- x$cov %>% as.list %>% dplyr::as_data_frame()

  # coo ---
  if (length(x$coo)==1 && is.na(x$coo))
    coo <- NULL
  else
    coo <- x$coo %>% lapply(list) %>% dplyr::as_data_frame()

  # bind all and turn into a data_frame ---
  dplyr::bind_cols(name, cov, coo) %>% dplyr::as_data_frame() %>% .append_class("mom_df")
}

#' @rdname momify
#' @export
momify <- function(x){

  moms <- x %>%
    lapply(momify1)

  # check homogeneity of colnames
  tcn <- moms %>% lapply(colnames) %>% unlist %>% table
  tcn <- tcn != length(moms)
  if (any(tcn)){
    .message_warning("some components were not found everywhere",
                     tcn %>% which %>% names)
  }
  res <- moms %>%
    dplyr::bind_rows() %>%
    .append_class("mom_df")

  # if coo are not named and if name is present, name them
  # if (is.null(names(res$coo)) && !is.null(res$name))
  #   names(res$coo) <- res$name

  # return this beauty
  res
}
