context("to_*")

test_that("to_mom", {
  is_ok <- function(x) x %>% parse_mom() %>% some_non_valid() %>% `!`
  expect_true(from_mom("bot_lite/") %>% to_mom() %>% is_ok)
  expect_true(from_mom("bot_lite.mom") %>% to_mom() %>% is_ok)
})

test_that("to_Out", {
  expect_true(from_mom("tests/testthat/bot_lite.mom") %>%
                to_Out() %>% Momocs::is_Out())
})
