context("parse utils")

test_that("print_premom", {
  expect_output(harvest("bot_lite") %>% parse_mom %>% print,
                "all lines tagged")
  expect_output(harvest("crash.mom") %>% parse_mom %>% print,
                "7 non valid")
})
