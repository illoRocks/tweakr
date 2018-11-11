
#' @importFrom purrr map
NULL


#' randomly
#'
#' Folding strategie
#'
#' @param data Training data
#' @param method Name of folding strategie.
#' \itemize{
#'   \item \strong{cv} Cross-Validation. k can be choosen (Default: 5)
#' }
#' @param ... further arguments depends on the method.
#'
#' @return List of indices in train data.
#'
#' @examples
#' set.seed(123)
#' folds <- randomly(iris, 'cv', k=5)
#'
#' @export
randomly <- function(data, method="cv", ...) {
  args <- list(...)

  if (method=="cv") {

    k <- get_value(args$k, 5)
    n <- nrow(data)
    in_test <- split(seq_len(n), sample(rep(1:k, length.out = n)))
    return(map(in_test, ~(1:n)[-.x]))

  }
}


