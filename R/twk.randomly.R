
#' @importFrom purrr map
NULL


#' randomly
#'
#' Folding strategie
#'
#' @param data Training data
#' @param sample_method Name of folding strategie.
#' \itemize{
#'   \item \strong{cv} Cross-Validation. k can be choosen (Default: 5)
#' }
#' @param k Number of folds.
#' @param ... further arguments depends on the method.
#'
#' @return List of indices in train data.
#'
#' @examples
#' set.seed(123)
#' folds <- randomly(iris, 'cv', k=5)
#'
#' @export
randomly <- function(data, sample_method="cv", k=5, ...) {

  if (sample_method=="cv") {

    n <- nrow(data)
    in_test <- split(seq_len(n), sample(rep(1:k, length.out = n)))
    return(map(in_test, ~(1:n)[-.x]))

  }

  stop("no valid sample method")
}


