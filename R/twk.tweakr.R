
#' @importFrom R6 R6Class
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate_if select summarise inner_join bind_cols group_by
#' @importFrom purrr pmap_dfr map_dfr

Tweakr <- R6Class(
  classname = "tweaker",
  public = list(

    # custom functions
    func_train = NULL,
    func_predict = NULL,
    func_eval = NULL,

    # iterations
    iterations_trained = NULL,

    # Initialize will create a Tweaker
    initialize = function(train_set,
                          params,
                          func_train,
                          func_predict,
                          func_eval,
                          k=5,
                          folds=NULL,
                          parallel_strategy=NULL) {

      # check for missing values
      if (missing(func_train) && missing(func_predict) && missing(func_eval))
        stop("you have to specify `func_train`, `func_predict`, `func_eval`")

      # check for missing values
      if (missing(train_set))
        stop("you have to specify `train_set`")

      # check for missing values
      if (is.null(k) && is.null(folds))
        stop("you have to specify `k` or `params`")

      # check for missing values
      if (is.null(params))
        stop("you have to specify `params`")

      if (all(names(formals(func_train)) != c("train","param")) ||
          all(names(formals(func_predict)) != c("fit","test")) ||
          all(names(formals(func_eval)) != c("pred","test")))
        stop("Wrong arguments: func_train(train, param); func_predict(fit, test); func_eval(pred, test)")

      # assign functions
      self$func_train <- func_train
      self$func_predict <- func_predict
      self$func_eval <- func_eval

      # assign params
      self$params <- params

      # assign data
      self$train_set <- train_set

      # folds for cross validation
      if(is.null(folds)) {
        n <- nrow(train_set)
        self$folds_in_test <- split(seq_len(n), sample(rep(1:k, length.out = n)))
      } else {
        self$folds_in_test <- folds
      }

    },

    # train model
    train_model = function() {

      do_train <- function(in_test, param, id, ...) {
        fit <- self$func_train(self$train_set[-in_test, ], param)
        tibble(id=id, fit=list(fit), in_test=list(in_test))
      }

      self$iterations_trained <- pmap_dfr(self$iterations, do_train)

    },

    # predict model
    predict_model = function() {

      do_predict <- function(in_test, param, id, fit, ...) {
        pred <- self$func_predict(fit, (self$train_set[in_test, ]))
        tibble(id=id, in_test=list(in_test), fit=list(fit), pred=list(pred))
      }

      self$iterations_trained <- pmap_dfr(self$iterations_trained, do_predict)

    },

    # eval model
    eval_model = function() {

      do_eval <- function(in_test, param, id, fit, pred, ...) {
        eval <- self$func_eval(pred, self$train_set[in_test, ])
        tibble(id=id, in_test=list(in_test), fit=list(fit), pred=list(pred), eval=eval)
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
    ..folds_in_test = NULL,
    ..train_set = NULL
  ),

  active = list(

    iterations = function() {

      pmap_dfr(self$params, function(id, param, ...) {
        map_dfr(self$folds_in_test, function(in_test) tibble(id=id, param=list(param), in_test=list(in_test)))
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

      print(value)
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

    folds_in_test = function(value) {

      if (missing(value))
        return(private$..folds_in_test)

      private$..folds_in_test <- value

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
#' @param func_train Function to train a model.
#' @param func_predict Function to predict the out of fold data.
#' @param func_eval Function to evaluate predictions.
#'
#' @export
tweakr <- function(train_set,
                   params,
                   k,
                   folds=NULL,
                   func_train,
                   func_predict,
                   func_eval) {

  twk <- Tweakr$new(train_set=train_set,
                    params=params,
                    k=k,
                    folds=folds,
                    func_train=func_train,
                    func_predict=func_predict,
                    func_eval=func_eval)

  twk$train_model()
  twk$predict_model()
  twk$eval_model()

  invisible(twk)
}







