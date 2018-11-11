
context("tweakr")

test_that("train rpart with iris data", {

  library(rpart)
  set.seed(123)

  twk <- tweakr(train_set = iris,
                params = list(cp=c(.01,.05)),
                k = 5,
                func_train = function(train, param) rpart(Species~. , train, control = rpart.control(cp = param$cp)),
                func_predict  = function(fit, test) predict(fit, test, type = "class"),
                func_eval = function(pred, test) sum(pred == test$Species) / nrow(test))

  expect_is(twk, "tweaker")
  expect_is(twk$result, "data.frame")
  expect_named(twk$result, c("eval","fit","pred","cp"))
  expect_equal(round(twk$result$eval, 2), c(0.92, 0.92))
  expect_length(twk$folds_in_train, 5)

})

test_that("test wrong arguments in functions", {

  expect_error(tweakr(train_set = iris,
                      params = list(cp=c(.01,.05)),
                      k = 5,
                      func_train = function(a, param) 1,
                      func_predict  = function(fit, test) 1,
                      func_eval = function(pred, test) 1))

})

test_that("test missing arguments in tweakr", {

  expect_error(tweakr(params = list(cp=c(.01,.05)),
                      k = 5,
                      func_train = function(a, param) 1,
                      func_predict  = function(fit, test) 1,
                      func_eval = function(pred, test) 1))

})

test_that("test tweak_sample", {

  folds1 <- randomly(iris,"cv", k=30)
  folds2 <- randomly(iris)

  expect_length(folds1, 30)
  expect_length(folds2, 5)
  expect_equal(range(unlist(folds1)), c(1,150))

})



