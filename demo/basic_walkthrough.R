library(rpart)
library(tweakr)
set.seed(123)

twk <- tweakr(
  train_set = iris,
  params = list(
    cp=c(.1,.001)
  ),
  func_train = function(train, param) {

    rpart(Species~. , train, control = rpart.control(cp = param$cp))

  },
  func_predict = function(fit, test) {

    predict(fit, test, type = "class")

  },
  func_eval = function(pred, test) {

    pred_class <- colnames(pred)[which.max(pred)]
    sum(pred_class == test$Species) / nrow(test)

  })

pred_prob <- predict(twk, iris)
