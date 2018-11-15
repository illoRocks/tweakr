#' @importFrom glue glue
NULL

# small wrapper to check argument names
check_arguments <- function(func, expected_names) {

  if(any(names(formals(func)) != expected_names))
    stop(paste0("Wrong arguments in ",deparse(substitute(func)),"(",paste(expected_names, collapse=", "),")"))

  return(NULL)
}

# small wrapper to check if parameter is missing
check_missing <- function(value) {

  if(missing(value) || is.null(value))
    stop(paste0("you have to specify `",deparse(substitute(func)),"`"))

  return(NULL)
}

# if null then get default value
get_value <- function(val, default) {

  if(is.null(val))
    return(default)
  else
    return(val)
}

# print in dependecne of the condition
glat_if <- function(condition, ..., .envir=parent.frame()) {

  if(condition)
    cat(glue(..., .envir=.envir), "\n")
}
