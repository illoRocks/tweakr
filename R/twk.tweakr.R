
#' @importFrom R6 R6Class
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate_if select summarise inner_join bind_cols group_by filter bind_rows arrange
#' @importFrom purrr pmap_dfr map_dfr map_chr
#' @importFrom readr write_rds

Tweakr <- R6Class(
  "tweakr",
  private = list(
    ..params = tibble()
  ),

  active = list(

    iterations = function() {

      pmap_dfr(self$params, function(.id, param, ...) {
        map_dfr(self$folds_in_train, function(in_train) tibble(.id=.id, param=list(param), in_train=list(in_train)))
      })

    },

    params = function(value) {

      if (missing(value))
        return(private$..params)

      new_params <- pmap_dfr(value, function(...) {
        tibble(param=list(list(...)), .id=paste(list(...), collapse="_"))
      })

      private$..params <- filter(new_params, !.id %in% private$..params[[".id"]])

    },
    result = function() {

      res <- summarise(group_by(self$iterations_history, .id), eval=mean(eval), fit=list(fit), pred=list(pred), in_train=list(in_train))
      res <- inner_join(res, self$params_history, by=".id")
      bind_cols(res, map_dfr(res$param, as_tibble))

    },
    best_fit = function() {

      res <- arrange(self$result, eval)[1,]
      list(eval=res$eval, param=res$param[[1]], fit=res$fit[[1]])

    }

  ),
  public = list(

    func_train = NULL,
    verbose = NULL,
    iterations_trained = NULL,
    iterations_history = tibble(),
    params_history = tibble(),
    train_set = NULL,
    folds_in_train = NULL,

    initialize = function(train_set,
                          params=NULL,
                          func_train,
                          folds=NULL,
                          parallel_strategy=NULL,
                          verbose=1) {

      # check parameters
      check_missing(train_set)
      check_missing(func_train)
      check_arguments(func_train, c("param","train","test"))

      # assign arguments
      self$func_train <- func_train
      self$train_set <- train_set
      self$verbose <- verbose
      self$folds_in_train <- folds
      if (!is.null(params)) self$params <- params

    },

    # train model
    train_model = function() {

      glat_if(self$verbose, "start training ...")

      do_train <- function(in_train, param, .id, ...) {
        res <- self$func_train(param, self$train_set[in_train, ], self$train_set[-in_train, ], ...)

        tibble(.id=.id,
               in_train=list(in_train),
               fit=list(res[["fit"]]),
               pred=list(res[["pred"]]),
               eval=get_value(res[["eval"]]))
      }

      self$iterations_trained <- pmap_dfr(self$iterations, do_train)

      self$iterations_history <- bind_rows(self$iterations_history,
                                           self$iterations_trained)

      self$params_history <- bind_rows(self$params_history, self$params)

    },

    print = function(...) {

      print(select(self$result, -.id), ...)

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
#' @param train_set Training data.
#' @param params List of parameters.
#' @param k Number of folds.
#' @param folds custom folds.
#' @param func_train Function to train a model. The arguments must be `param`, `train` and `test` and return the fitted object.
#' @param save_path The path where the model are stored. (Default: NULL)
#' @param save_freq The frequence of model saving. (Defaut: 10)
#' @param twk_object tweakr object to continue training.
#' @param ... Additional arguments for tweekr functions.
#'
#' @examples
#'
#' library(rpart)
#' set.seed(123)
#'
#'  twk <- tweakr(train_set = iris,
#' params = list(cp=c(.01,.05)),
#' k = 10,
#' func_train = function(param, train, test) {
#'   model <- rpart(Species~. , train, control = rpart.control(cp = param$cp))
#'   pred <- predict(model, test, type = "class")
#'   error <- sum(pred == test$Species) / nrow(test)
#'   list(fit=model, pred=pred, eval=error)
#' })
#'
#' prediction <- predict(twk,
#'                       iris,
#'                       func_predict = function(fit, test) predict(fit, test, type = "prob"))
#'
#' @export
tweakr <- function(train_set,
                   params,
                   func_train,
                   k=5,
                   folds=NULL,
                   save_path=NULL,
                   save_freq=10,
                   twk_object=NULL,
                   ...) {

  args <- list(...)
  params <- paramize(params, ...)
  folds <- if(is.null(folds)) randomly(train_set, k=k, ...) else folds
  verbose <- get_value(args$verbose, 1)

  glat_if(verbose,
          "folding strategy: {get_value(args$sample_method, 'cv')} (folds: {length(folds)})\n",
          "number of iterations: {nrow(params)} (parameters) x {length(folds)} (folds)\n")

  if (is.null(twk_object)) {
    twk <- Tweakr$new(train_set=train_set,
                      folds=folds,
                      func_train=func_train,
                      verbose=verbose)
  } else {
    twk <- twk_object
  }

  param_seq <- seq_len(nrow(params))
  if (is.null(save_path))
    param_indices <- list(param_seq)
  else
    param_indices <- split(param_seq, ceiling(param_seq/save_freq))

  for (i in param_indices) {

    twk$params <- params[i,]

    twk$train_model()

    if (!is.null(save_path))
      write_rds(twk, save_path)

  }

  invisible(twk)
}







