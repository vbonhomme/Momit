context("from_*")

test_that("from_mom", {
  expect_is(from_mom("bot_lite"), "mom_df")
  expect_is(from_mom("bot_lite.mom"), "mom_df")
  expect_is(from_mom("empty.mom"), "mom_df")
  expect_is(from_mom("mini.mom"), "mom_df")
  expect_is(from_mom("olea.mom"), "mom_df")
  expect_is(from_mom("out.mom"), "mom_df")
  expect_is(from_mom("3parts.mom"), "mom_df")
  expect_true(harvest("crash.mom") %>% parse_mom() %>% some_non_valid())
})

# 30LM fails! TODO
test_that("from_tps", {
    expect_equal(list.files(full=T, pattern="allowen") %>%
                   lapply(from_tps) %>%
                   lapply(class) %>%
                   sapply(`[`, 1) %>%
                   unique(), "mom_df")
})


test_that("from_nts", {
  expect_is(harvest("tpsDig_XYsusSEAsia.nts")  %>% from_nts(), "mom_df")
  # ~20 s below
  # expect_is(harvest("foreign/tpsDig_guenons_online.nts")  %>%
  #             lapply(function(x) gsub("([[:digit:]]+_[[:digit:]])", "ind_\\1", x)) %>%
  #             from_nts(), "mom_df")
})

test_that("from_lmk", {
  expect_is("meshtools_ZMK_TRF_01_34.lmk" %>% from_lmk, "mom_df")
})

test_that("from_stv", {
  expect_is("meshtools_TRF_01_34.stv" %>% from_stv, "mom_df")
})

test_that("from_StereoMorph", {
  expect_equal(list.files(full=T, pattern="Stereo") %>%
    lapply(from_StereoMorph) %>%
    lapply(class) %>%
    sapply(`[`, 1) %>%
    unique(), "mom_df")
})

test_that("from_Optimas", {
  expect_equal(lapply(list.files(pattern="Optimas", full.names = T, rec=T),
                from_Optimas) %>%
                 lapply(class) %>%
                 sapply(`[`, 1) %>%
                 unique(), "mom_df")
})

test_that("from_PAST", {
  expect_is(harvest("PAST_ontogeny9L.past.txt") %>% from_PAST(), "mom_df")
})
