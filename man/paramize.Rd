% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/twk.paramize.R
\name{paramize}
\alias{paramize}
\title{paramize}
\usage{
paramize(params, search_method = "grid", search_len = 5, ...)
}
\arguments{
\item{params}{list containing parameters.}

\item{search_method}{search method can be grid or random.}

\item{search_len}{length of random paramters.}

\item{...}{not used}
}
\value{
data.frame with parameters.
}
\description{
Create data.frame of parameters.
}
\examples{
paramize(list(a=c(1,2),b=c(3,4)))
paramize(data.frame(a=c(1,2),b=c(3,4)))
paramize(c(1,2))
paramize(list(a=c("c","d"),b=c(3,4)), "random", 10)

}
