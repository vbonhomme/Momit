# prune, split and lapply parse1

context("momification")

test_that("momify1", {
  harvest("tests/testthat/3parts.mom")  %>%
    # in case we have a list (eg several files at the beginning)
    do.call("c", .) %>%
    .prune %>%
    .split_on("~")  %>%  `[[`(1)  %>% .remove_non_valid() %>% sift  -> x
  # mom_list
  expect_is(x, "mom_list")
  # now full mom_df with 3 parts on $coo
  expect_is(x %>% momify1(), "mom_df")

  # below test for none, some, all components
  x_full <- x
  expect_is(x_full %>% momify1(), "mom_df")
  # name
  x_full$name <- NA
  expect_is(x_full %>% momify1(), "mom_df")
  # name, coo
  x_full$coo <- NA
  expect_is(x_full %>% momify1(), "mom_df")
  # name, coo, cov
  x_full$cov <- NA
  expect_is(x_full %>% momify1(), "mom_df")

  # coo
  x_full <- x
  x_full$coo <- NA
  expect_is(x_full %>% momify1(), "mom_df")

  # coo, cov
  x_full$coo <- NA
  expect_is(x_full %>% momify1(), "mom_df")

  # cov
  x_full <- x
  x_full$cov <- NA
  expect_is(x_full %>% momify1(), "mom_df")
})

