context("harvest functions")

test_that("harvest", {
  lf <- list.files(recursive=T, pattern="mom$")
  expect_equal(length(harvest(pattern="mom$")), length(lf))
  none_empty <- harvest(pattern="mom$") %>% do.call("c", .)
  some_empty <- harvest(pattern="mom$", prune=FALSE) %>% do.call("c", .)
  expect_true(sum(nchar(none_empty)==0)==0)
  expect_true(sum(nchar(some_empty)==0)>0)
})
