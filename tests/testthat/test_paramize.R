context("paramize")

test_that("test paramize",{

  params1 <- data.frame(a=c("c","d"),b=c(3,4))
  params2 <- list(a=c(1,2),b=c(3,4))
  params3 <- c(1,2,3)
  params4 <- list(a=c("c","d"),b=c(3,4))

  expect_identical(paramize(params1), as_tibble(params1))
  expect_equal(nrow(paramize(params2)), 4)
  expect_equal(nrow(paramize(params3)), 3)
  expect_equal(nrow(paramize(params4, search_method="random", search_len=10)), 10)
})

test_that("test paramize types",{

  params <- paramize(list(a.dbl=c(0, 1), b.int=c(0, 1)), search_method = "random")

  expect_type(params$a, "double")
  expect_type(params$b, "integer")

})
