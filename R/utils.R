
# regex patterns ------------------------------------------
number <- "-?[[:digit:]]+\\.?[[:digit:]]*(e(\\+|-))?[[:space:]]*"
name   <- "[[:alpha:]]+([[:alnum:]]|[[:punct:]])*"

coordinates <- paste0("^(", number, ")+$")
cov_categ   <- paste0("^", name, " ", name, "$")
cov_quant   <- paste0("^", name, " ", number, "$")
partition   <- paste0("^", name, "$")
collated    <- "^~[[:alnum:]]+.*$"

patterns_regex <- c("coordinates" = coordinates,
                    "cov_categ"   = cov_categ,
                    "cov_quant"   = cov_quant,
                    "partition"   = partition,
                    "collated"    = collated)

# shape translaters ----------------------------------------
# turns a list with <cov_name value> into a data.frame
.str_2_df <- function(x){
  x <- strsplit(x, " ")
  x %>%
    lapply(function(.) paste(.[-1], collapse=" ")) %>%
    `names<-`(sapply(x, `[`, 1)) %>%
    as_tibble()
}

# turns the first line of a data.frame
# into a space separated vector of character
.df_2_str <- function(x){
  cn <- colnames(x)
  x %>% seq_along() %>% sapply(function(.) paste(cn[.], x[1, .]))
}

# turns a list with coordinates as a vector of characters into a matrix
.str_2_mtx <- function(x){
  x %>%
    strsplit(" ") %>%
    lapply(as.numeric) %>%
    do.call("rbind", .)
}

# turns a matrix into a vector of coordinates as character
.mtx_2_str <- function(x){
  # for the (rare) case where scientific notation
  # is retained when as.character
  op <- options(scipen=999)
  on.exit(options(op))
  apply(x, 1, paste0, collapse=" ")
}

# turns a single line from a mom_df into a mom
.mom_df_2_mom <- function(x){
  c(
    x$name %>% as.character %>% paste0("~", .),
    x$coo[[1]] %>% .mtx_2_str(),
    x[, -(1:2)] %>% .df_2_str()
  )
}

# utils ----------------------------------------------------

# domestic and internal functions -------------------------

# removes on a string all non valid lines
.prune <- function(x){
  # remove empty lines
  x <- x[nchar(x)>0]
  x %>%
    # 2+ spaces are converted to a single space
    gsub(" {2, }", " ", .) %>%
    # remove leading and trailing spaces
    gsub("^ | $", "", .) #%>%
  # every lines not containing (one or more) (word or number)
  #grep("[[:alnum:]]+", ., value=TRUE)
}

# read one or several files, ensure a name is present or add it,
# bind all character together
.readLines_and_bind <- function(x){
  x %>%
    lapply(readLines) %>%
    .ensure_collated(n=.trim_path_and_extension(x)) %>%
    do.call("c", .)
}

# when collating a single or many files,
# ensure a collated name (ie ~name) is present,
# otherwise add it to the character
.ensure_collated <- function(x, n){
  missing_collated <- which(sapply(x, function(.) !any(grepl(collated, .))))
  if (length(missing_collated)==0){
    return(x)
  } else {
    lapply(missing_collated, function(.) x[[.]] <- c(paste0("~", n[.]), x[[.]])) %>%
      return()

  }
}

# turns a vector with NAs and non-NAs
# replace NAs with the last non-NA
# borrowed from user2100721@stackoverflow
.replace_na_with_last<-function(x){
  y <- !is.na(x)
  x[which(y)[c(1, 1:sum(y))][cumsum(y)+1]]
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


# use each first element of a list to name it, then remove it
.name_list_from_first_and_remove <- function(x){
  names(x) <- sapply(x, `[`, 1)
  lapply(x, `[`, -1)
}

# split where what=="collated"
.split_collated <- function(x){
  if (!any(x$what=="collated"))
    return(x)
  f <- .splitting_vector(x$what, "collated")
  dfs <- split(x, f)
  names(dfs) <- x$x[grep("collated", x$what)]
  dfs
}

# # trim path and extension (if any)
.trim_path_and_extension <- function(x){
  x %>%
    strsplit("/") %>% sapply(function(.) .[length(.)]) %>%
    gsub("\\.[[:alpha:]]{2,3}$", "", .)
}


# fridge --------------------------------------------------

# # detects valid paths
# .is_path <- function(x){
#   file.exists(x) && length(x)==1
# }

# # use a named list to add partition names
# .add_partition_name_from_component_name <- function(x){
#   lapply(seq_along(x), function(i) c(names(x)[i], x[[i]]))
# }


