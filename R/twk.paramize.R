#' @importFrom purrr map map_dfc
#' @importFrom dplyr select_if
NULL

#' paramize
#'
#' Create data.frame of parameters.
#'
#' @param params list containing parameters.
#' @param search_method search method can be grid or random.
#' @param search_len length of random paramters.
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
paramize <- function(params, search_method="grid", search_len=NULL) {

  if(is.data.frame(params))
    return(params)

  if(!is.list(params) && is.vector(params))
    return(data.frame(param1=params))

  if(!is.list(params))
    stop("params should be a list, data.frame or vector")

  if(search_method=="grid")
    return(expand.grid(params))

  if(search_method=="random") {

    if(is.null(search_len))
      stop("search_len has to be specified")

    params <- as_tibble(params)
    res <- bind_cols(map_dfc(select_if(params,is.numeric), ~runif(search_len, min(.x), max(.x))),
                     map_dfc(select_if(params,~!is.numeric(.x)), sample, size=search_len, replace = T))
    return(res)
  }

  warning("Wrong arguments! Return NULL")
  NULL
}

