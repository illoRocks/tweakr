#' @importFrom purrr map map_dfc
#' @importFrom dplyr select_if mutate_at vars ends_with
#' @importFrom tibble as_tibble
#' @importFrom stringr str_remove
#'
NULL

#' paramize
#'
#' Create data.frame of parameters.
#'
#' @param params list containing parameters.
#' @param search_method search method can be grid or random.
#' @param search_len length of random paramters.
#' @param ... not used
#'
#' @return data.frame with parameters.
#'
#' @examples
#' paramize(list(a=c(1,2),b=c(3,4)))
#' paramize(data.frame(a=c(1,2),b=c(3,4)))
#' paramize(c(1,2))
#' paramize(list(a=c("c","d"),b=c(3,4)), "random", 10)
#'
#' @export
paramize <- function(params, search_method="grid", search_len=5, ...) {

  convert_types <- function(x) {
    x <- mutate_at(x, vars(ends_with('.int')), as.integer)
    x <- mutate_at(x, vars(ends_with('.dbl')), as.double)
    x <- mutate_at(x, vars(ends_with('.chr')), as.character)
    x <- mutate_at(x, vars(ends_with('.fct')), as.factor)
    colnames(x) <- str_remove(colnames(x), ".int|.dbl|.chr|.fct")
    x
  }

  if(is.data.frame(params))
    return(as_tibble(params))

  if(!is.list(params) && is.vector(params))
    return(tibble(param1=params))

  if(!is.list(params))
    stop("params should be a list, data.frame or vector")

  if(search_method=="grid") {
    res <- mutate_if(as_tibble(expand.grid(params)), is.factor, as.character)
    res <- convert_types(res)
    return(res)
  }

  if(search_method=="random") {

    if(is.null(search_len))
      stop("search_len has to be specified")

    params <- as_tibble(params)
    res <- bind_cols(map_dfc(select_if(params,is.numeric), ~runif(search_len, min(.x), max(.x))),
                     map_dfc(select_if(params,~!is.numeric(.x)), sample, size=search_len, replace = T))

    res <- convert_types(res)

    return(res)
  }

  warning("Wrong search_method in paramize!")
  NULL
}

