% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geodetector.R
\name{risk_detector}
\alias{risk_detector}
\title{risk detector}
\usage{
risk_detector(y, x, alpha = 0.95)
}
\arguments{
\item{y}{Variable Y, continuous numeric vector.}

\item{x}{Covariate X, \code{factor}, \code{character} or \code{discrete numeric}.}

\item{alpha}{(optional) Confidence level of the interval, default is \code{0.95}.}
}
\value{
A tibble.
}
\description{
Determine whether there is a significant difference between the attribute means of two sub regions.
}
\examples{
risk_detector(y = 1:7,
              x = c('x',rep('y',3),rep('z',3)))

}
\author{
Wenbo Lv \email{lyu.geosocial@gmail.com}
}
