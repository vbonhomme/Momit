# general cleaning
# - empty lines
# - tabs
# -multiple spaces
# given a pattern split a list on it
prepolish <- function(x){
  # remove empty lines
  x <- x[stringr::str_length(x)>0]

  x %>%
    # turn tab(s) to spaces
    stringr::str_replace("(\t)+", " ") %>%
    # 2+ spaces are converted to a single space
    stringr::str_replace(" {2,}", " ") %>%
    # remove leading and trailing spaces
    stringr::str_remove("^ | $")
}

# given a string, split on pattern
strsplit_on <- function(x, pattern){
  split(x, .splitting_vector(x, pattern))
}

# turns a vector of characters,
# with some elements beginning with a pattern,
# split it into a list
# c("a", "b", "ab", "ac", "b") %>% .split_where_pattern("a")
.splitting_vector <- function(x, pattern){
  # detect pattern and prepare single partition
  ids <- stringr::str_which(x, pattern)

  # early return if nothing detected
  if (length(ids)==0)
    return(rep(1, length(x)))

  # otherwise prepare a splitting vector
  f   <- rep(NA_integer_, length(x))

  # other complete the f
  f[ids] <- seq_len(length(ids))

  y <- !is.na(f)
  cumsum(y)+1
}

# turns a vector with NAs and non-NAs
# replace NAs with the last non-NA
# borrowed from user2100721@stackoverflow
# .replace_na_with_last<-function(x){
#   y <- !is.na(x)
#   x[which(y)[c(1, 1:sum(y))][cumsum(y)+1]]
# }
