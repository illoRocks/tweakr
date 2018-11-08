# small wrapper to check argument names
check_arguments <- function(func, expected_names) {

  if(any(names(formals(func)) != expected_names))
    stop(paste0("Wrong arguments in ",deparse(substitute(func)),"(",paste(expected_names, collapse=", "),")"))

  return(NULL)
}

# small wrapper to check if parameter is missing
check_missing <- function(func) {

  if(missing(func))
    stop(paste0("you have to specify `",deparse(substitute(func)),"`"))

  return(NULL)
}
