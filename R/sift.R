# empty mom_list
.mom_list <- function(){
  list(name=NA_character_,
       coo=NA_real_,
       cov=NA_character_) %>%
    .append_class("mom_list")
}

#' Filter components of morphometric data
#'
#' For a single individual, morphometric data can be: name, coordinates or associated covariates.
#' All can be present, some (or even all) can be absent. This function takes such a single individual,
#' as polished lines of text that stick to `.mom` file format definition, typically prepared by [parse], and
#' filter existing components. You should not need it directly unless you write your own `parse/from` functions.
#'
#' @param x lines of text, typically in the `.mom` format and prepared by [parse]
#' @return `sift` returns a `mom_list`, a list with these components:
#' * `$name` `character` or `NA` if not provided
#' * `$coo` a list of partitions, that are matrices of coordinates (aka shapes) or `NA` if not provided
#' * `$cov` a named vector of covariates or `NA` if not provided
#'
#' `mom_list` can be turned into a `mom_df` with [momify].
#'
#' `sift_{name, coo, cov}` each return a `list` with two components:
#' * `$x`, which is the provided `x` minus the extracted information (if any)...
#' * now in the proper format in `$y`
#' @name sift
#' @rdname sift
#' @export
sift <- function(x){
  res <- .mom_list()
  # name ---
  tmp <- x %>% sift_name()
  res$name <- tmp$y
  x <- tmp$x
  # cov ---
  tmp <- x %>% sift_cov()
  res$cov <- tmp$y
  x <- tmp$x
  # coo ---
  tmp <- x %>% sift_coo()
  res$coo <- tmp$y
  x <- tmp$x
  # return this beauty
  res
}

#' @rdname sift
#' @export
sift_name <- function(x){
  # to return results
  res <- list(x=x, y=NA_character_)
  name_ids <- grepl(name_regex, x)
  if (sum(name_ids)>0){
    # more than one name case
    if (sum(name_ids)>1){
      .message_warning("more than one name found, first is retained",
                       x[name_ids])
    }
    # below (not above) retain the first name
    res$y <- x[name_ids] %>% `[`(1) %>% gsub("~", "", .)
    res$x <- x[!name_ids] #%>% .append_class("name")
  }
  res
}

#' @rdname sift
#' @export
sift_cov <- function(x){
  # to return results
  res <- list(x=x, y=NA_character_)
  # find cov patterns
  cov_ids <- grepl(cov_regex, x)
  # if any, extract cov values and name them
  if (sum(cov_ids)>0){
    cov        <- x[cov_ids] %>% strsplit(" ") %>% lapply(`[`, -1) %>% sapply(paste, collapse= " ")
    names(cov) <- x[cov_ids] %>% strsplit(" ") %>% sapply(`[`, 1)
    # message for non-unique cov_names
    tn <- table(names(cov))
    if (any(tn>1)){
      .message_warning("non unique names for covariates",
                       names(tn[which(tn>1)]))
    }
    res$y <- cov %>% .append_class("cov")
    res$x   <- x[!cov_ids]
  }
  return(res)
}

#' @rdname sift
#' @export
sift_coo <- function(x){
  # to return results
  res <- list(x=x, y=NA_real_)
  # find coo or partition patterns
  coo_ids <- grepl(paste(coo_regex, partition_regex, sep="|"), x)
  # case where no coo
  if (sum(coo_ids)==0)
    return(res)
  # otherwise treat them
  y <- x[coo_ids]
  # ensure the first line is a partition name, otherwise add it
  if (!grepl("^[[:alpha:]]", y[1]))
    y <- c("coo", y)
  # split on partition patterns
  ys <- y %>% .split_on('[[:alpha:]]') %>%
    .name_list_from_first_and_remove()

  # check all or some empty partitions
  pl <- sapply(ys, length)
  if (all(pl==0)){
    .message_warning("only empty partitions")
    return(res)
  }
  if (any(pl==0)){
    .message_warning("some empty partitions",
                     names(ys)[pl==0])
    ys <- ys[pl!=0]
  }

  # check for coo formatting
  non_valid <- lapply(ys, function(.x) !grepl(coo_regex, .x))
  if (any(sapply(non_valid, any))){
    .message_warning("some non valid coo",
                     lapply(ys, function(.x) grep(coo_regex, .x, value=TRUE, invert=TRUE)) %>% unlist)
    ys <- lapply(ys, function(.x) grep(coo_regex, .x, value=TRUE))
  }
  # check for dimension homogeneity within partitions
  heterodim <- sapply(ys, function(.x) .x %>% strsplit(" ") %>% sapply(length) %>% unique %>% length)
  heterodim <- heterodim != 1
  if (any(heterodim)){
    .message_error("these partitions were not homogeneous in terms of coo dimensions; fix it please",
                   names(ys)[heterodim])
    res$y <- list()
    return(res)
  }

  # finally, continue and grab coos
  ys <- ys %>%
    # then turn into coordinates and coo
    lapply(.str_2_mtx) %>%
    lapply(.append_class, class_to_add="coo")
  # proper and unique names
  names(ys) <- .make_unique(names(ys))
  # fill res and return it
  res$y <- ys
  res$x <- x[!coo_ids]
  return(res)
}
