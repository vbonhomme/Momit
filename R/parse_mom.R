
# parse ----------------------------------------------------

# Parse a single mom
# x character from readLines
# return a mom_df
.parse_1_mom <- function(x){
  x <- .prune(x)
  patterns_regex %>%
    lapply(grepl, x) %>%
    as.data.frame %>%
    `colnames<-`(names(patterns_regex)) %>%
    dplyr::mutate(non_valid =! rowSums(.)) %>%
    dplyr::transmute(x=x, what=colnames(.)[apply(., 1, which)]) %>%
    tibble::as_tibble() -> x
  class(x) <- unique(c("yo", class(x)))
  x
}

#' Parse a mom-like vector of character lines
#'
#' @param x `character`, typically lines returned by [harvest]
#' or similarly prepared with [readLines].
#' @return `mom_df`
#' @export
parse_mom <- function(x){
  # single file case
  if (is.character(x)){
    .parse_1_mom(x) %>%
      return()
  }
  # several files case
  if (is.list(x)) {
    lapply(x, .parse_1_mom) %>%
      do.call("rbind", .) %>%
      return()
  }
}

# print method for mom_df
#' @export
print.yo <- function(x, ...){
  # counts the number of non_valid lines
  NV <- sum(x$what == "non_valid")
  if (NV>0) { # some non_valid cases
    x %>%
      do.call(dplyr::bind_cols, .) %>% print
    cat("\n", crayon::red("\u2717", NV, "non valid line(s)\n"))
  } else {     # zero non_valid case
    # no idea why this doesnt work
    # x %>% dplyr::as_data_frame() %>% print
    x %>% do.call(dplyr::bind_cols, .) %>% print
    y <- table(x$what)
    cat("\n")
    paste0("* ", y, "\t", names(y), "\n") %>%
      crayon::silver() %>%
      cat(sep = "")
    cat(crayon::green("\u2713 all lines tagged"))
  }
}

# mom -----------------------------------------------------

# takes a single premom_df and return a single mom as a list
# used internally by momify
.pre_mom <- function(x){
  # if any removes collated
  x <- dplyr::filter(x, what != "collated")

  # pick cov (if any) and remove them
  .cov <- grep("cov", x$what)
  if (length(.cov)>0){
    cov <- x$x[.cov] %>% .str_2_df()
    x <- x %>% dplyr::slice(-.cov)
  } else {
    cov <- NULL
  }

  # handles remaining, eg coordinates and partitions (if any)
  # first removes everything else
  x <- dplyr::filter(x, what %in% c("partition", "coordinates"))

  if (any(grepl("partition", x$what))){ # partition(s) case
    coo <- x$x %>%
      split(.splitting_vector(x$what, "partition")) %>%
      .name_list_from_first_and_remove() %>%
      lapply(.str_2_mtx)
  } else { # no partition case
    coo <- x$x %>% .str_2_mtx()
  }
  # return extracted components
  list(coo=coo, cov=cov)
}

#' Convert a tag_df to a mom_df
#' @param x a tag_df
#' @export
momify <- function(x){

  # check that zero non_valid lines are present
  NV <- dplyr::filter(x, what == "non_valid")
  if (nrow(NV)>0){
    # no idea why this doesn't work
    # NV %>% dplyr::as_data_frame %>% print
    NV %>% do.call(dplyr::bind_cols, .) %>% print(n=50)
    stop(crayon::red("\u2713", nrow(NV), "non valid line(s)\n"))
  }

  #split into a list and name it
  x <- .split_collated(x)
  # turn each tibble1 into mom
  moms <- lapply(x, .pre_mom)

  # if not named (ie not collated), dummy name individuals
  # if named, remov tildes
  if (is.null(names(moms))){
    name <- paste0("shp", 1:length(moms))
  } else {
    name <- gsub("~", "", names(moms))
  }

  # bind components and turn them into a mom_df tibble
  coo <- lapply(moms, `[`, "coo") %>% do.call("rbind", .)
  cov <- sapply(moms, `[`, "cov") %>% do.call("rbind", .)

  cbind(name, coo, cov) %>%
    dplyr::as_data_frame() %>%
    `class<-`(unique(c("mom_df", class(.))))
}
