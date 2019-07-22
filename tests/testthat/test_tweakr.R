context("tweakr")

library(rpart)
set.seed(123)

func_train <- function(param, train, test) {
  model <- rpart(Species~. , train, control = rpart.control(cp = param$cp))
  pred <- predict(model, test, type = "class")
  error <- sum(pred == test$Species) / nrow(test)
  list(fit=model, pred=pred, eval=error)
}

test_that("train rpart with iris data", {

  twk <- tweakr(train_set = iris,
                params = list(cp=c(.01,.05)),
                k = 10,
                func_train = func_train)

  prediction <- predict(twk,
                        iris,
                        func_predict = function(fit, test) predict(fit, test, type = "prob"))

  expect_is(twk, "tweakr")
  expect_is(twk$result, "data.frame")
  expect_true(all(c("eval","fit","pred") %in% colnames(twk$result)))
  expect_equal(round(twk$result$eval, 2), c(0.94, 0.95))
  expect_length(twk$folds_in_train, 10)

})

test_that("test output", {

  expect_output(tweakr(train_set = iris,
                       params = list(a=c(1)),
                       func_train = func_train),
                ".*folds: 5.*iterations: 1.*")

  expect_output(tweakr(train_set = iris,
                       params = list(a=c(1)),
                       verbose=0,
                       func_train = func_train),
                NA)

})

test_that("test wrong arguments in functions", {

  expect_error(tweakr(train_set = iris,
                      k = 5,
                      func_train = func_train))

})

test_that("test missing arguments in tweakr", {

  expect_error(tweakr(params = list(cp=c(.01,.05)),
                      k = 5,
                      func_train = func_train))

})
