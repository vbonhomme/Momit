
# given a pattern split a list on it
.split_on <- function(x, pattern){
  if (sum(grepl(pattern, x))==0){
    .message_warning("nothing to split on (eg ~name)")
    return(list(x))
  }
  split(x, .splitting_vector(x, pattern))
}

# turns a vector of characters,
# with some elements beginning with a pattern,
# split it into a list
# c("a", "b", "ab", "ac", "b") %>% .split_where_pattern("a")
.splitting_vector <- function(x, pattern){
  ids <- grep(pattern, x)
  # more than 1 partition
  f <- rep(NA, length(x))
  f[ids] <- seq_len(length(ids))
  .replace_na_with_last(f)
}
