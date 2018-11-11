
#' @importFrom R6 R6Class
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate_if select summarise inner_join bind_cols group_by
#' @importFrom purrr pmap_dfr map_dfr map_chr

Tweakr <- R6Class(
  classname = "tweaker",
  public = list(

    # custom functions
    func_train = NULL,
    func_predict = NULL,
    func_eval = NULL,
    verbose = NULL,

    # iterations
    iterations_trained = NULL,

    # Initialize will create a Tweaker
    initialize = function(train_set,
                          params,
                          func_train,
                          func_predict,
                          func_eval,
                          k=5,
                          sample_method="cv",
                          folds=NULL,
                          parallel_strategy=NULL,
                          verbose=1) {

      # check for missing values
      check_missing(train_set)
      check_missing(func_train)
      check_missing(func_predict)
      check_missing(func_eval)

      # check for wrong arguments
      check_arguments(func_train, c("train","param"))
      check_arguments(func_predict, c("fit","test"))
      check_arguments(func_eval, c("pred","test"))

      # check for missing values
      if (is.null(k) && is.null(folds))
        stop("you have to specify `k` or `folds`")

      # check for missing values
      if (is.null(params))
        stop("you have to specify `params`")

      # assign arguments
      self$func_train <- func_train
      self$func_predict <- func_predict
      self$func_eval <- func_eval
      self$params <- params
      self$train_set <- train_set
      self$verbose <- verbose

      # folds for cross validation
      if(is.null(folds)) {
        self$folds_in_train <- randomly(train_set, sample_method, k)
      } else {
        self$folds_in_train <- folds
      }

      # show infos
      glat_if(self$verbose,
             "folding strategy: {sample_method} (folds: {length(self$folds_in_train)})\n",
             "number of iterations: {nrow(self$params)} (parameters) x {length(self$folds_in_train)} (folds)\n")

    },

    # train model
    train_model = function() {

      do_train <- function(in_train, param, id, ...) {
        fit <- self$func_train(self$train_set[in_train, ], param)
        tibble(id=id, fit=list(fit), in_train=list(in_train))
      }

      self$iterations_trained <- pmap_dfr(self$iterations, do_train)

      if(any(map_chr(self$iterations_trained$fit, class) %in% c("numeric","character")))
        warning("returned element of train function is not a model")
    },

    # predict model
    predict_model = function() {

      do_predict <- function(in_train, param, id, fit, ...) {
        pred <- self$func_predict(fit, (self$train_set[-in_train, ]))
        tibble(id=id, in_train=list(in_train), fit=list(fit), pred=list(pred))
      }

      self$iterations_trained <- pmap_dfr(self$iterations_trained, do_predict)

    },

    # eval model
    eval_model = function() {

      do_eval <- function(in_train, param, id, fit, pred, ...) {
        eval <- self$func_eval(pred, self$train_set[-in_train, ])
        tibble(id=id, in_train=list(in_train), fit=list(fit), pred=list(pred), eval=eval)
      }

      self$iterations_trained <- pmap_dfr(self$iterations_trained, do_eval)

    },

    print = function() {

      print(self$result)

      invisible(self)
    }

  ),

  # private parameters
  private = list(
    ..params = tibble(),
    ..folds_in_train = NULL,
    ..train_set = NULL
  ),

  active = list(

    iterations = function() {

      pmap_dfr(self$params, function(id, param, ...) {
        map_dfr(self$folds_in_train, function(in_train) tibble(id=id, param=list(param), in_train=list(in_train)))
      })

    },

    train_set = function(value) {

      if (missing(value))
        return(private$..train_set)

      private$..train_set <- value

    },

    params = function(value) {

      if (missing(value))
        return(private$..params)

      if (is.list(value))
        value <- expand.grid(value)

      value <- as_tibble(value)
      value <- mutate_if(value, is.factor, as.character)
      value <- select(value, order(colnames(value)))

      value <- pmap_dfr(value, function(...) {
        tibble(param=list(list(...)), id=paste(list(...), collapse="_"))
      })

      private$..params <- value

    },

    folds_in_train = function(value) {

      if (missing(value))
        return(private$..folds_in_train)

      private$..folds_in_train <- value

    },

    result = function(value) {

      if (!missing(value))
        stop("$result is read only")

      res <- summarise(group_by(self$iterations_trained, id), eval=mean(eval), fit=list(fit), pred=list(pred))
        res <- inner_join(res, self$params, by="id")
        res <- bind_cols(res, map_dfr(res$param, as_tibble))
        select(res, -param, -id)

    }

  )
)


#' tweakr
#'
#' Parametertuning
#'
#' Parametrtuning for custom models.
#'
#' @param train_set Training data
#' @param params List of parameters
#' @param k Number of folds.
#' @param folds custom folds.
#' @param func_train Function to train a model. The arguments must be `train` and `param`.
#' @param func_predict Function to predict the out of fold data. The arguments must be `fit` and `test`.
#' @param func_eval Function to evaluate predictions. The arguments must be `pred` and `test`.
#' @param run Functions should be excecuted or not.
#' @param ... Additional arguments for Tweakr
#'
#' @examples
#'
#' library(rpart)
#' set.seed(123)
#'
#' twk <- tweakr(train_set = iris,
#'               params = list(cp=c(.01,.05)),
#'               k = 5,
#'               func_train = function(train, param)
#'                 rpart(Species~. , train, control = rpart.control(cp = param$cp)),
#'               func_predict  = function(fit, test)
#'                 predict(fit, test, type = "class"),
#'               func_eval = function(pred, test)
#'                 sum(pred == test$Species) / nrow(test))
#'
#' @export
tweakr <- function(train_set,
                   params,
                   k=5,
                   folds=NULL,
                   func_train,
                   func_predict,
                   func_eval,
                   run=TRUE,
                   ...) {

  twk <- Tweakr$new(train_set=train_set,
                    params=params,
                    k=k,
                    folds=folds,
                    func_train=func_train,
                    func_predict=func_predict,
                    func_eval=func_eval,
                    ...)

  if(run) {
    twk$train_model()
    twk$predict_model()
    twk$eval_model()
  }

  invisible(twk)
}








