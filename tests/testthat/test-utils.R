context("domestic functions")

test_that(".prune", {
  x <- harvest("3parts.mom")[[1]] %>%  .prune()
  expect_length(x %>% grep("^ | $| {2,}", .), 0)
  expect_length(grep("[[:alnum:]]+", x), length(x))
  expect_length(which(nchar(x)==0), 0)
})

test_that(".str_2_mtx", {
  x <- c("766 991", "704 1046")
  expect_true(x %>%  .str_2_mtx %>% is.matrix)
})

test_that(".mtx_2_str", {
  expect_true(matrix(1:12, 6, 2) %>% .mtx_2_str %>% is.character)
})


test_that(".splitting_vector", {
  expect_equal(c("a", "b", "ab", "ac", "b") %>% .splitting_vector("a"),
               c(1, 1, 2, 3, 3))
})

test_that(".split_on", {
  x <- c("56 76", "45 32")
  expect_output(.split_on(x, "plop"))
  expect_is(.split_on(x, "plop"), "list")
  expect_length(.split_on(x, "plop"), 1)

  x <- c("plop", x)
  expect_is(.split_on(x, "plop"), "list")
  expect_length(.split_on(x, "plop"), 1)

  x <- c(x, x, x)
  expect_is(.split_on(x, "plop"), "list")
  expect_length(.split_on(x, "plop"), 3)

})


test_that(".name_list_from_first_and_remove", {
  x <- list(c("plop", "1", "2", "3"), c("plip", "4", "5", "6")) %>% .name_list_from_first_and_remove()
  expect_equal(names(x), c("plop", "plip"))
  expect_equal(x[[1]], c("1", "2", "3"))
  expect_equal(x[[2]], c("4", "5", "6"))
})

test_that(".name_if_none", {
  x <- c("56 76", "45 32")
  expect_false(any(grep("~", x)))
  expect_true(any(grep("~", .name_if_none(x))))
})

test_that(".messages", {
  expect_output(.message_ok("yopiyo"))
  expect_output(.message_ok("yopiyo", letters[1:5]))

  expect_output(.message_warning("yopiyo"))
  expect_output(.message_warning("yopiyo", letters[1:5]))

  expect_output(.message_error("yopiyo"))
  expect_output(.message_error("yopiyo", letters[1:5]))
})

test_that(".make_unique", {
  is_unique <- function(x){
    length(x) == length(unique(x))
  }
  expect_true(is_unique(.make_unique(letters[1])))
  expect_true(is_unique(.make_unique(letters[1:5])))
  expect_true(is_unique(.make_unique(rep(letters[1], 100))))
  expect_true(is_unique(.make_unique(rep(letters[1:5], 10))))
})


# test_that(".trim_path_and_extension", {
#   x <- "plop/plip/myfile.txt" %>% .trim_path_and_extension()
#   expect_equal(x, "myfile")
# })
