
#' @importFrom R6 R6Class
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate_if select summarise inner_join bind_cols group_by filter
#' @importFrom purrr pmap_dfr map_dfr map_chr
#' @importFrom progress progress_bar
#' @importFrom readr write_rds

Tweakr <- R6Class("tweaker",

  private = list(
    ..params = tibble()
  ),

  active = list(

    iterations = function() {

      pmap_dfr(self$params, function(id, param, ...) {
        map_dfr(self$folds_in_train, function(in_train) tibble(id=id, param=list(param), in_train=list(in_train)))
      })

    },

    params = function(value) {

      if (missing(value))
        return(private$..params)

      if (is.null(value))
        return(NULL)

      new_params <- pmap_dfr(value, function(...) {
        tibble(param=list(list(...)), id=paste(list(...), collapse="_"))
      })

      private$..params <- filter(new_params, !id %in% private$..params[["id"]])

    },

    result = function(value) {

      if (!missing(value))
        stop("$result is read only")

      res <- summarise(group_by(self$iterations_trained, id), eval=mean(eval), fit=list(fit), pred=list(pred))
      res <- inner_join(res, self$params, by="id")
      res <- bind_cols(res, map_dfr(res$param, as_tibble))
      select(res, -param, -id)

    }

  ),
  public = list(

    func_train = NULL,
    func_predict = NULL,
    func_eval = NULL,
    verbose = NULL,
    iterations_trained = NULL,
    train_set = NULL,
    folds_in_train = NULL,

    initialize = function(train_set,
                          params=NULL,
                          func_train,
                          func_predict,
                          func_eval,
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

      # assign arguments
      self$func_train <- func_train
      self$func_predict <- func_predict
      self$func_eval <- func_eval
      self$train_set <- train_set
      self$verbose <- verbose
      self$folds_in_train <- folds
      self$params <- params

    },

    # train model
    train_model = function() {

      if(self$verbose)
        pb <- progress_bar$new(format="train model [:bar] :percent current: :current  eta: :eta", total = nrow(self$iterations))

      do_train <- function(in_train, param, id, ...) {
        fit <- self$func_train(self$train_set[in_train, ], param)
        if(self$verbose) pb$tick()
        tibble(id=id, fit=list(fit), in_train=list(in_train))
      }

      self$iterations_trained <- pmap_dfr(self$iterations, do_train)

      if(self$verbose)
        pb$terminate()

      if(any(map_chr(self$iterations_trained$fit, class) %in% c("numeric","character")))
        warning("returned element of train function is not a model")
    },

    # predict model
    predict_model = function() {

      if(self$verbose)
        pb <- progress_bar$new(format="predict test [:bar] :percent current: :current  eta: :eta", total = nrow(self$iterations))

      do_predict <- function(in_train, param, id, fit, ...) {
        pred <- self$func_predict(fit, (self$train_set[-in_train, ]))
        if(self$verbose) pb$tick()
        tibble(id=id, in_train=list(in_train), fit=list(fit), pred=list(pred))
      }

      self$iterations_trained <- pmap_dfr(self$iterations_trained, do_predict)

      if(self$verbose)
        pb$terminate()

    },

    # eval model
    eval_model = function() {

      if(self$verbose)
        pb <- progress_bar$new(format="evaluate model [:bar] :percent current: :current  eta: :eta", total = nrow(self$iterations))

      do_eval <- function(in_train, param, id, fit, pred, ...) {
        eval <- self$func_eval(pred, self$train_set[-in_train, ])
        if(self$verbose) pb$tick()
        tibble(id=id, in_train=list(in_train), fit=list(fit), pred=list(pred), eval=eval)
      }

      self$iterations_trained <- pmap_dfr(self$iterations_trained, do_eval)

      if(self$verbose)
        pb$terminate()

    },

    print = function() {

      print(self$result)

      invisible(self)
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
#' @param func_train Function to train a model. The arguments must be `train` and `param` and return the fitted object.
#' @param func_predict Function to predict the out of fold data. The arguments must be `fit` and `test` and return the predicted values.
#' @param func_eval Function to evaluate predictions. The arguments must be `pred` and `test` and return a single metric.
#' @param save_path The path where the model are stored. (Default: NULL)
#' @param save_freq The frequence of model saving. (Defaut: 10)
#' @param ... Additional arguments for tweekr functions.
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
                   save_path=NULL,
                   save_freq=10,
                   ...) {

  args <- list(...)
  params <- paramize(params, ...)
  folds <- if(is.null(folds)) randomly(train_set, k=k, ...) else folds
  verbose <- get_value(args$verbose, 1)

  glat_if(verbose,
          "folding strategy: {get_value(args$sample_method, 'cv')} (folds: {length(folds)})\n",
          "number of iterations: {nrow(params)} (parameters) x {length(folds)} (folds)\n")

  twk <- Tweakr$new(train_set=train_set,
                    folds=folds,
                    func_train=func_train,
                    func_predict=func_predict,
                    func_eval=func_eval,
                    verbose=verbose)

  param_seq <- seq_len(nrow(params))
  if (is.null(save_path))
    param_indices <- list(param_seq)
  else
    param_indices <- split(param_seq, ceiling(param_seq/save_freq))

  j <- 0
  for (i in param_indices) {

    twk$params <- params[i,]

    twk$train_model()
    twk$predict_model()
    twk$eval_model()

    if (!is.null(save_path))
      write_rds(twk, paste0(save_path))

  }

  invisible(twk)
}







