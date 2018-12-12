context("randomly")


test_that("test randomly", {

  folds1 <- randomly(iris,"cv", k=30)
  folds2 <- randomly(iris)

  expect_length(folds1, 30)
  expect_length(folds2, 5)
  expect_equal(range(unlist(folds1)), c(1,150))

})
