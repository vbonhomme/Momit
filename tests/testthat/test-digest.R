test_that("digest works", {

  # digest helpers
  expect_true(.is_unique(letters[1:5]))
  expect_false(.is_unique(c(1, 1, 2)))

  expect_true(.is_constant(rep(1, 10)))
  expect_false(.is_constant(c(1.1, rep(1, 10))))


  # numeric
  dig_num <- 1:5 %>% digest
  dig_numNA <- c(1:5, NA) %>% digest
  expect_equal(nrow(dig_num), 3)
  expect_equal(nrow(dig_numNA), 4)

  # factor
  dig_Species <- digest(iris$Species)
  expect_equal(nrow(dig_Species), 3)
  expect_true(all(dig_Species$count==50))
  SpeciesNA <- iris$Species
  SpeciesNA[5] <- NA
  dig_SpeciesNA <- digest(SpeciesNA)
  expect_equal(nrow(dig_SpeciesNA), 4)
  expect_true(all(dig_SpeciesNA$count==c(1, 49, 50, 50)))
  expect_output(print(digest(factor(letters[1:5]))), "unique")

  # character
  dig_chr <- digest(c("a", letters[1:4]))
  expect_equal(nrow(dig_chr), 4)
  expect_true(all(dig_chr$count==c(2, 1, 1, 1)))
  expect_output(print(digest(letters[1:5])), "unique")

  # mom

})
