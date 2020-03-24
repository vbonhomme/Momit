context("harvest functions")

test_that("harvest", {
  lf <- list.files(recursive=T, pattern="mom$")
  expect_equal(length(harvest(pattern="mom$")), length(lf))

  # many
  expect_is(harvest(pattern="mom$"), "list")
  # just one
  expect_is(harvest(pattern="tests/testthat/mini.mom"), "list")
})
