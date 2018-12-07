
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

  twk2 <- tweakr(train_set = iris,
                params = list(cp=c(.01,.05)),
                k = 50,
                func_train = function(train, param) rpart(Species~. , train, control = rpart.control(cp = param$cp)),
                func_predict  = function(fit, test) predict(fit, test, type = "class"),
                func_eval = function(pred, test) sum(pred == test$Species) / nrow(test))

  prediction <- predict(twk, iris, func_predict = function(fit, test) predict(fit, test, type = "prob"))

  expect_error(predict(twk, iris), regexp="`func_predict` should return numeric values. Use custom function in predict method.")
  expect_is(twk, "tweakr")
  expect_is(twk$result, "data.frame")
  expect_true(all(c("eval","fit","pred") %in% colnames(twk$result)))
  expect_equal(round(twk$result$eval, 2), c(0.92, 0.92))
  expect_length(twk$folds_in_train, 5)
  expect_length(twk2$folds_in_train, 50)

})

test_that("test output", {

  expect_output(tweakr(train_set = iris,
                       params = list(a=c(1)),
                       func_train = function(train, param) lm(as.numeric(Species)~.,train),
                       func_predict  = function(fit, test) 1,
                       func_eval = function(pred, test) 1),
                ".*folds: 5.*iterations: 1.*")

  expect_output(tweakr(train_set = iris,
                       params = list(a=c(1)),
                       verbose=0,
                       func_train = function(train, param) lm(as.numeric(Species)~.,train),
                       func_predict  = function(fit, test) 1,
                       func_eval = function(pred, test) 1),
                NA)

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

test_that("test randomly", {

  folds1 <- randomly(iris,"cv", k=30)
  folds2 <- randomly(iris)

  expect_length(folds1, 30)
  expect_length(folds2, 5)
  expect_equal(range(unlist(folds1)), c(1,150))

})



