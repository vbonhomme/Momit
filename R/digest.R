#' Digest tibbles and vector content
#'
#' A kind of improved [summary]
#'
#' For `numeric` columns, returns a tiny `summary`.
#' For `character`/`factor` columns, returns a count table.
#' For the two previous cases tests if values are constant or unique, respectively.
#' In both cases, returning a tibble would not be useful,
#' but knowing that a `character`/`factor` is unique can be useful
#' when making an `id` column.
#' Also report NA cases, if any.
#' For tibbles, prints a colwise digest.
#'
#' @param x `tibble` or `vector`
#'
#' @examples
#' # numeric
#' 1:5 %>% digest
#' c(NA, 1:5) %>% digest() # count NA
#' rep(1, 5) %>% digest()  # constant
#'
#' # factor
#' iris$Species %>% digest
#' c(NA, iris$Species) %>% digest() # count NA
#' factor(rep("a", 5)) %>% digest() # unique
#' @export
digest <- function(x){
  UseMethod("digest")
}


#' @export
digest.default <- function(x){
  ifelse(.is_unique(x), "unique", "?")
}

#' @export
digest.factor <- function(x){
  # early return if unique
  if (.is_unique(x))
    return(paste0("unique (", length(x), ")"))

  # count, reserve, remove NAS
  nas <- sum(is.na(x))
  x <- stats::na.omit(x)

  # table and tibble
  res <- x %>%
    table() %>%
    tibble::as_tibble() %>%
    purrr::set_names(c("level", "count"))

  # put back NAs, if any
  # NA so important that they come first
  if (nas>0)
    res <- dplyr::bind_rows(tibble::tibble(level="NA", count=nas),
                            res)

  # return this beauty
  res
}

# possibly annoying but appears useful
#' @export
digest.character <- function(x){
  x %>% factor() %>% digest()
}

#' @export
digest.numeric <- function(x){
  # appears stupid but help fs_bytes (among others) to be a real double
  x <- as.numeric(x)

  # early return if unique
  if (.is_constant(x))
    return(paste0("constant (", length(x), ")"))

  # count, reserve, remove NAS
  nas <- sum(is.na(x))
  x <- stats::na.omit(x)

  # begins the summary
  tibble::tribble(
    ~quantile,  ~value,
    "min",      min(x),
    "median",   median(x),
    "max",      max(x)
  ) -> res

  # quantile for NA is not the best name ever
  # NA so important that they come first
  if (nas>0)
    res <- dplyr::bind_rows(tibble::tibble(quantile=NA, value=nas),
                            res)

  # return this beauty
  res
}

#' @export
digest.mom_tbl <- function(x){
  purrr::iwalk(x, ~{
    cli::cli_h1(.y)  # the column name
    print(digest(.x)) # the digest itself
  })
}

# digest_utils --------------------------------------------
.is_unique <- function(x){
  # early case when  NA detected
  if (any(is.na(x))) return(FALSE)
  # otherwise tests if the number of levels equals vector length
  ifelse(length(x)==length(unique(x)), TRUE, FALSE)
}

.is_constant <- function(x){
  # early case when  NA detected
  if (any(is.na(x))) return(FALSE)
  # use var coeff to test if constant
  ifelse((stats::sd(x)/stats::mean(x)) < 1e-10, TRUE, FALSE)
}
