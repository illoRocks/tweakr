#' @importFrom purrr map
NULL

#' @name predict.tweakr
#' @aliases predict
#'
#' Predicted values based on tweakr object.
#'
#' @usage
#' ## S3 method for class 'tweakr'
#' predict(object, new_data, type=NULL)
#'
#' @inheritParams predict
#' @param new_data An data frame in which to look for variables with which to predict.
#' @param type character string denoting the type of predicted value returned.
#'
#' @examples
#'
#' library(rpart)
#'
#' twk <- tweakr(iris,
#'               params = list(cp=c(.1,.001)),
#'               func_train = function(train, param) {
#'                 rpart(Species~. , train, control = rpart.control(cp = param$cp))},
#'               func_predict = function(fit, test){
#'                 predict(fit, test, type = "prob")},
#'               func_eval = function(pred, test) {
#'                 pred_class <- colnames(pred)[which.max(pred)]
#'                 sum(pred_class == test$Species) / nrow(test)})
#'
#' pred_prob <- predict(twk, iris)
#' pred_class <- predict(twk, iris, "class")
#'
#'
#' @export
#'
predict.tweakr <- function(object, new_data, type=NULL, func_predict=NULL, ...) {

  if(is.null(func_predict))
    func_predict <- object$func_predict

  x <- map(object$best_fit$fit, func_predict, test=new_data)
  y <- do.call(cbind, x)

  if(is.character(x[[1]]))
    stop("`func_predict` should return numeric values. Use custom function in predict method.")

  if (is.vector(x[[1]])) {
    z <- rowMeans(y)

  } else {
    y <- array(y, dim=c(dim(x[[1]]), length(x)))
    z <- colMeans(aperm(y, c(3, 1, 2)), na.rm = TRUE)
    colnames(z) <- colnames(x[[1]])
    if (!is.null(type) && type=="class")
      z <- colnames(z)[apply(z, 1, which.max)]

  }

  return(z)
}
