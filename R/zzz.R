
# regex constants -----------------------------------------
number <- "-?[[:digit:]]+\\.?[[:digit:]]*(e(\\+|-))?[[:space:]]*"
name   <- "[[:alpha:]]+([[:alnum:]]|[[:punct:]])*"
cov_regex <- "^[[:alpha:]]+ [[:alnum:]]+.*$"
coo_regex <- paste0("^(", number, ")+$")
name_regex <- "^~[[:alnum:]]+.*$"
partition_regex <- paste0("^[[:alpha:]]+([[:alnum:]]|_)*$")
# collate all valid regex
valid_regex <- paste(name_regex, coo_regex, cov_regex, partition_regex, sep=")|(") %>%
  paste0("(", ., ")")

# shape translaters ----------------------------------------
# turns a list with <cov_name value> into a data.frame
.str_2_df <- function(x){
  x <- strsplit(x, " ")
  x %>%
    lapply(function(.) paste(.[-1], collapse=" ")) %>%
    `names<-`(sapply(x, `[`, 1)) %>%
    dplyr::as_data_frame()
}

# turns the first line of a data.frame
# into a space separated vector of character
.df_2_str <- function(x){
  cn <- colnames(x)
  seq_along(x) %>% sapply(function(.) paste(cn[.], x[1, .]))
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


# domestic and internal functions -------------------------

# append a class if does not belong to it yet
.append_class <- function (x, class_to_add) {
  if (!(class_to_add %in% class(x)))
    class(x) <- c(class_to_add, class(x))
  x
}

# removes on a string all non valid lines
.prune <- function(x){
  # check for encoding problems, then remove empty lines
  nx <- try(nchar(x))
  # check
    if (class(nx)=="try-error"){
      .message_error("possible encoding problem, see ?harvest")
      stop()
    }
  x <- x[nx>0]
  x %>%
    # tab(s) to spaces
    gsub("(\t)+", " ", .) %>%
    # 2+ spaces are converted to a single space
    gsub(" {2, }", " ", .) %>%
    # remove leading and trailing spaces
    gsub("^ | $", "", .)
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

.name_if_none <- function(x, dummyname="~NA"){
if (sum(grepl("~", x)==0))
  x <- c(dummyname, x)
x
}


# # # trim path and extension (if any)
# .trim_path_and_extension <- function(x){
#   x %>%
#     strsplit("/") %>% sapply(function(.) .[length(.)]) %>%
#     gsub("\\.[[:alpha:]]{2,3}$", "", .)
# }
#


# printers ------------------------------------------------
# print.coo <- function(x){
#   n <- ncol(x)
#   dimnames(x) <- list(paste0(1:nrow(x), ":   "),
#                       letters[24:(24+n-1)])
#
#   if (nrow(x)>6){
#     utils::print(head(x, 6))
#     cat("...(+", nrow(x)-6, ")\n", sep="")
#   } else {
#     x %>% `class<-`("matrix") %>% print
#   }
# }

# messages ------------------------------------------------
.message_ok <- function(mess, details=""){
  crayon::green(cli::symbol$tick, " ", mess, sep="") %>% cat()
  if (!missing(details))
    crayon::green(":\n\t", paste(details, collapse="\n\t"), "\n", sep="") %>% cat()
}

.message_warning <- function(mess, details=""){
  crayon::yellow(cli::symbol$pointer, " ", mess, sep="") %>% cat()
  if (!missing(details))
    crayon::yellow(":\n\t", paste(details, collapse="\n\t"), "\n", sep="") %>% cat()
}

.message_error <- function(mess, details=""){
  crayon::red(cli::symbol$cross, " ", mess, sep="") %>% cat()
  if (!missing(details))
    crayon::red(":\n\t", paste(details, collapse="\n\t"), "\n", sep="") %>% cat()
}

# .message_ok("yopiyo")
# .message_ok("yopiyo", letters[1:5])
#
# .message_warning("yopiyo")
# .message_warning("yopiyo", letters[1:5])
#
# .message_error("yopiyo")
# .message_error("yopiyo", letters[1:5])

# checking -----
.remove_non_valid <- function(x){
  non_valid <- !grepl(valid_regex, x)
  if (any(non_valid)){
    .message_warning("at least non valid lines found, these were excluded",
                     x[non_valid])
    return(x[!non_valid])
  } else {
    return(x)
  }
}

.split_on <- function(x, pattern){
  if (sum(grepl(pattern, x))==0){
    .message_warning("nothing to split on (eg ~name)")
    return(list(x))
  }
  split(x, .splitting_vector(x, pattern))
}

.make_unique <- function(x){
  # if unique names, return as it is
  if (length(x) == length(unique(x))){
    return(x)
  }
  # otherwise make them unique
  tx <- table(x)
  # non-unique names
  nun <- names(tx)[which(tx>1)]
  for (i in nun){
    ids <- which(x==i)
    x[ids] <- paste0(x[ids], "_", seq_along(ids))
  }
  return(x)
}

# exotic format brushes -----------------------------------
.shorten_coordinates <- function(x, ncol){
  pattern_lines <- grep(coo_regex, x)
  x[pattern_lines] <- x[pattern_lines] %>%
    .str_2_mtx() %>% `[`(, 1:ncol) %>% .mtx_2_str()
  x
}

.on_top <- function(x, pattern){
  pos <- grep(pattern, x)
  c(x[pos], x[-pos])
}
