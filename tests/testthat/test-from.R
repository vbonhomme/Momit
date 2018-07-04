context("from_*")

test_that("from_mom", {
  x <- from_mom("bot_lite/")
  expect_is(x, "mom_df")
  expect_true(nrow(x)==10)

  x <- from_mom("bot_lite.mom")
  expect_is(x, "mom_df")
  expect_true(nrow(x)==20)


  expect_output( x <- from_mom("mini.mom"))
  expect_true(nrow(x)==1)
  cn <- "coo" %in% colnames(x)
  expect_true(length(cn)==1 && cn)

  expect_error(from_mom("empty.mom"))

  expect_is(from_mom("out.mom"), "mom_df")

  expect_is( x <- from_mom("3parts.mom"), "mom_df")
  expect_true(sum(sapply(x, is.list))==3)
  expect_is(from_mom("olea.mom"), "mom_df")

  expect_output(harvest("crash.mom")[[1]] %>% Momit:::.remove_non_valid())
  # for a more silent testing
  expect_output(x <- try(from_mom("crash.mom"), silent=T))
  expect_true(class(x)=="try-error")

})
#
# # 30LM fails! TODO
# test_that("from_tps", {
#   expect_equal(list.files(full=T, pattern="allowen") %>%
#                  lapply(from_tps) %>%
#                  lapply(class) %>%
#                  sapply(`[`, 1) %>%
#                  unique(), "mom_df")
# })
#
# test_that("from_tps", {
#   expect_equal(list.files(full=T, pattern="allactaga") %>%
#                  lapply(from_tps) %>%
#                  lapply(class) %>%
#                  sapply(`[`, 1) %>%
#                  unique(), "mom_df")
#
# })
#
#
# test_that("from_nts", {
#   expect_is(harvest("tpsDig_XYsusSEAsia.nts")  %>% from_nts(), "mom_df")
#   # ~20 s below
#   # expect_is(harvest("foreign/tpsDig_guenons_online.nts")  %>%
#   #             lapply(function(x) gsub("([[:digit:]]+_[[:digit:]])", "ind_\\1", x)) %>%
#   #             from_nts(), "mom_df")
# })
#
# test_that("from_lmk", {
#   expect_is("meshtools_ZMK_TRF_01_34.lmk" %>% from_lmk, "mom_df")
# })
#
# test_that("from_stv", {
#   expect_is("meshtools_TRF_01_34.stv" %>% from_stv, "mom_df")
# })
#
# # test_that("from_StereoMorph", {
# #   expect_equal(list.files(full=T, pattern="Stereo") %>%
# #     lapply(from_StereoMorph) %>%
# #     lapply(class) %>%
# #     sapply(`[`, 1) %>%
# #     unique(), "mom_df")
# # })
#
# # test_that("from_Optimas", {
# #   expect_equal(lapply(list.files(pattern="Optimas", full.names = T, rec=T),
# #                 from_Optimas) %>%
# #                  lapply(class) %>%
# #                  sapply(`[`, 1) %>%
# #                  unique(), "mom_df")
# # })
#
# test_that("from_PAST", {
#   expect_is(harvest("PAST_ontogeny9L.past.txt") %>% from_PAST(), "mom_df")
# })
