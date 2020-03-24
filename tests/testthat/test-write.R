# context("write")
#
# test_that("write_single", {
#   from_mom("bot_lite") %>% to_mom %>% write_single("test_single.mom")
#   expect_true(file.exists("test_single.mom"))
#   expect_is(from_mom("test_single.mom"), "mom_df")
#   file.remove("test_single.mom")
#
#   from_mom("bot_lite") %>% to_mom %>% write_separately("test_separately", "mom")
#   expect_true(dir.exists("test_separately"))
#   expect_is(from_mom("test_separately"), "mom_df")
#   file.remove(list.files("test_separetely", full=T))
#   unlink("test_separately", recursive = TRUE, force=TRUE)
# })
