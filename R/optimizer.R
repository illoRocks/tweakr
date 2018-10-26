library(purrr)
library(dplyr)
library(furrr)

cross_optimize <- function(data, params, func_train, func_predict, func_eval, k=5, folds=NULL, parallel_strategy=sequential) {
  
  if (missing(data) && 
      missing(params) &&
      missing(func_train) &&
      missing(func_predict) && 
      missing(func_eval) &&
      is.null(k) &&
      is.null(params)) 
    stop("you have to specify `data`, `params`, `func_train`, `func_predict`, `func_eval`, `k` and `params`")
  
  
  # folds for cross validation
  if(is.null(folds)) {
    n <- nrow(data)
    folds_in_test <- split(seq_len(n), sample(rep(1:k, length.out = n)))
  } else {
    folds_in_test <- folds
  }
  
  # map params
  if("data.frame" %in% class(params)) {
    params <- mutate_if(params, is.factor, as.character)
    params_list <- pmap(params, list)
  } else {
    params_list <- params
  }
  
  # future plan
  plan(parallel_strategy)
  
  # generate iterations
  iterations <- map_dfr(params_list, function(p, ...) {
    map_dfr(folds_in_test, function(i) tibble(in_test=lst(i), param=lst(p), id=paste(p, collapse="_")))
  })
  
  # train
  iterations <- future_pmap_dfr(iterations, function(in_test, param, id, ...) {
    fit <- func_train(data[-in_test,], param)
    tibble(id=id, in_test=lst(in_test), param=lst(param), fit=lst(fit))
  }, .progress = TRUE)
  
  # predict
  iterations <- pmap_dfr(iterations, function(in_test, param, fit, id, ...) {
    pred <- func_predict(fit, data[in_test,])
    tibble(id=id, in_test=lst(in_test), param=lst(param), fit=lst(fit), pred=lst(pred))
  }, .progress = TRUE)
  
  # error
  iterations <- pmap_dfr(iterations, function(in_test, param, fit, pred, id, ...) {
    error <- func_eval(pred, data[in_test,])
    tibble(id=id, in_test=lst(in_test), param=lst(param), fit=lst(fit), pred=lst(pred), error=error)
  })
  
  # return(iterations)
  sum_iter <-summarise(group_by(iterations, id), error=mean(error), in_test=lst(in_test), fit=list(fit), pred=lst(pred), param=param[1])
  sum_iter <- bind_cols(select(sum_iter, -id, -param), map_dfr(sum_iter$param, as_tibble))
  res <- list(iterations=sum_iter, 
              func_predict=func_predict, 
              func_train=func_train, 
              func_eval=func_eval,
              folds=folds_in_test,
              best_pred=arrange(sum_iter, error)$pred[[1]],
              best_fit=arrange(sum_iter, error)$fit[[1]])
  class(res) <- "optimizer"
  res
}

print.optimizer <- function(x) {
  x$iterations %>%  
    select(-in_test, -fit, -pred) %>% 
    arrange(error) %>% 
    print
  invisible(x)
}

rbind.optimizer <- function(...) {
  x <- list(...)
  xx <- map(x, ~.x$iterations)
  xx <- c(iterations=list(bind_rows(xx)), x[[1]][c("func_predict", "func_train", "func_eval")])
  class(xx) <- "optimizer"
  xx
}

predict.optimizer <- function(res, new_data, ...) {
  map_dfc(arrange(res$iterations, error)$fit[[1]], res$func_predict, test = new_data, ...)
}

# library(rpart)
# 
# source("optimizer.R")
# 
# plan(multisession)
# 
# res <- cross_optimize(
#   data=iris,
#   params=data.frame(cp=c(.9, 0.01)),
#   k=5,
#   parallel=T,
#   func_train = function(train, param) {
#     # hier steht train und param zu verfügung, fit model muss zurück gegeben werden
#     
#     rpart(Species~. , train, control = rpart.control(cp = param$cp))
#     
#   }, 
#   func_predict = function(fit, test, type = "class"){
#     # hier steht test und fit zu verfügung, prediction vector muss zurück gegeben werden
#     
#     predict(fit, test, type = type)
#     
#   }, 
#   func_eval = function(pred, test){
#     # hier steht test und pred zu verfügung, fehlerrate muss zurück gegeben werden
#     
#     sum(pred == test$Species) / nrow(test)
#     
#   })
# 
# predict(res, iris, type="class")










