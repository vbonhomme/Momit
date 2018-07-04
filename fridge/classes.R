
# moml ----------------------------------------------------
moml <- function(){
  list(name=NA_character_,
       coo=NA_real_,
       cov=NA_character_) %>%
    .append_class("mom_list")
}


# printers ------------------------------------------------
# print.name <- function(x){
#   cat(x, "\n")
# }
#
# print.cov <- function(x){
#   paste(names(x), "=", x, collapse="\n") %>% cat
# }


# print.moml <- function(x){
#   if (is_coo(x)){
#     print(x$coo)
#   }
#   if (is_coo(x)){
#     print(x$cov)
#   }
# }

# is_* ----------------------------------------------------
# is_coo <- function(x){
#   !is.null(x$coo)
# }
#
# is_cov <- function(x){
#   !is.null(x$cov)
# }

# is_mom_list <- function(x){
#   any(class(x)=="mom_list")
# }
#
# is_mom_df <- function(x){
#   any(class(x)=="mom_df")
# }


# is_unit <- function(x){
#   !is.null(attr(x, which="unit"))
# }
#
# unit <- function(x) {
#   attr(x, which="unit")
# }
#
# `unit<-` <- function(x, value){
#   attr(x, which="unit") <- value
# }
