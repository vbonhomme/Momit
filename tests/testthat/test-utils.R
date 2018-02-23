context("domestic functions")

test_that(".prune", {
  x <- readLines("3parts.mom") %>%  .prune()
  expect_length(x %>% grep("^ | $| {2,}", .), 0)
  expect_length(grep("[[:alnum:]]+", x), length(x))
})

test_that(".str_2_mtx", {
  x <- c("766 991", "704 1046")
  expect_true(x %>%  .str_2_mtx %>% is.matrix)
})

test_that(".mtx_2_str", {
  expect_true(matrix(1:12, 6, 2) %>% .mtx_2_str %>% is.character)
})

test_that(".str_2_df", {
  x <- c("plop plip", "plup 45", "plap p l o p") %>% .str_2_df()
  expect_true(is.data.frame(x) && ncol(x)==3)
})

test_that(".df_2_str", {
  x <- data.frame(plop=1, plip="ee 56 ff") %>% .df_2_str()
  expect_true(is.character(x))
  expect_length(x, 2)
})

test_that(".mom_df_2_mom", {
  mom_df <- from_mom("bot_lite")
  expect_is(mom_df[1, ] %>% .mom_df_2_mom(), "character")
  re_mom <- mom_df[2, ] %>% .mom_df_2_mom() %>% parse_mom()
  expect_equal(sum(re_mom[, 2]=="non_valid"), 0)
})

.ensure_collated <- function(x, n){
  missing_collated <- which(sapply(x, function(.) !any(grepl(collated, .))))
  if (length(missing_collated)==0){
    return(x)
  } else {
    lapply(missing_collated, function(.) x[[.]] <- c(paste0("~", n[.]), x[[.]])) %>%
      return()

  }
}

test_that(".replace_na_with_last", {
  x <- c("3", NA, NA, NA, "4", NA)
  expect_equal(sum(is.na(.replace_na_with_last(x))), 0)
})

test_that(".splitting_vector", {
  expect_equal(c("a", "b", "ab", "ac", "b") %>% .splitting_vector("a"),
                   c(1, 1, 2, 3, 3))
})

test_that(".name_list_from_first_and_remove", {
  x <- list(c("plop", "1", "2", "3"), c("plip", "4", "5", "6")) %>% .name_list_from_first_and_remove()
  expect_equal(names(x), c("plop", "plip"))
  expect_equal(x[[1]], c("1", "2", "3"))
  expect_equal(x[[2]], c("4", "5", "6"))
})

test_that(".split_collated", {
  lf <- list.files("bot_lite", full=T)
  x <- lf %>% lapply(readLines) %>% parse_mom %>% .split_collated()
  expect_is(x, "list")
  expect_equal(length(x), length(lf))
})

test_that(".trim_path_and_extension", {
  x <- "plop/plip/myfile.txt" %>% .trim_path_and_extension()
  expect_equal(x, "myfile")
})
