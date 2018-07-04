#' Turns mom_list(s) into mom_df
#'
#' Turns `mom_list` returned by [parse]rs into `data_frame`s, much easier to handle.
#' Most of the time you should not need it directly expect if you write your own [parse] function.
#'
#' @param x `character`or `list` of `character` for `momify1` and `momify`, respectively.
#' @return `mom_df`
#'
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
