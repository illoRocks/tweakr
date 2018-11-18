library(rpart)
set.seed(123)


func_train <- function(train, param)
  rpart(Species~. , train, control = rpart.control(cp = param$cp, minsplit = param$minsplit))

func_predict  <- function(fit, test)
  predict(fit, test, type = "class")

func_eval <- function(pred, test)
  sum(pred == test$Species) / nrow(test)

param <- list(cp=c(.02,.05,.06,.07,.08))#,minsplit=15:25)

twk <- tweakr(iris,
              params = param,
              func_train = func_train,
              func_predict = func_predict,
              func_eval = func_eval,
              save_path = NULL)#"demo/tweak.rds")
twk$params
twk$iterations_trained

tweakr(train_set = iris,
       params = param,#list(cp=c(.01,.05)),
       k = 5,
       func_train = function(train, param) rpart(Species~. , train, control = rpart.control(cp = param$cp)),
       func_predict  = function(fit, test) predict(fit, test, type = "class"),
       func_eval = function(pred, test) sum(pred == test$Species) / nrow(test))

params <- paramize(param)
split(seq_len(nrow(params)), ceiling(seq_len(nrow(params))/save_freq))

twk <- Tweakr$new(train_set=iris,
                  folds=folds,
                  func_train=func_train,
                  func_predict=func_predict,
                  func_eval=func_eval,
                  verbose=verbose)

twk$params <- params

if(run) {
  twk$train_model()
  twk$predict_model()
  twk$eval_model()
}

