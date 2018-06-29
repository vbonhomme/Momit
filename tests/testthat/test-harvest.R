context("harvest functions")

test_that("harvest", {
  lf <- list.files(recursive=T, pattern="mom$")
  expect_equal(length(harvest(pattern="mom$")), length(lf))
  none_empty <- harvest(pattern="mom$") %>% do.call("c", .)
})
