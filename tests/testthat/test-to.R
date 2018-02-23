context("to_*")

test_that("to_mom", {
  is_ok <- function(x) x %>% parse_mom() %>% some_non_valid() %>% `!`
  expect_true(from_mom("bot_lite/") %>% to_mom() %>% is_ok)
  expect_true(from_mom("bot_lite.mom") %>% to_mom() %>% is_ok)
})

test_that("to_Out", {
  expect_true(from_mom("bot_lite.mom") %>%
                to_Out() %>% Momocs::is_Out())
})

test_that("to_Opn", {
  expect_true(from_mom("olea.mom") %>%
                to_Opn() %>% Momocs::is_Opn())
})

test_that("to_Ldk", {
  expect_true(from_tps("tpsDig_allowen1.tps") %>%
                to_Ldk() %>% Momocs::is_Ldk())
})
