% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_lm.R
\name{my_lm}
\alias{my_lm}
\title{Linear Model function}
\usage{
my_lm(formula, data)
}
\arguments{
\item{formula}{Formula class object being used}

\item{data}{Data frame containing data used within \code{formula}}
}
\value{
Table containing columns with Estimate, Std. Error, t value and Pr(>|t|)
}
\description{
This function creates and fits a linear model
}
\examples{
my_lm(dist ~ speed, data = cars)

}
\keyword{inference,}
\keyword{prediction}
