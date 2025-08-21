\name{z.conf}
\alias{z.conf}

\title{ Confdidence interval for mean using z-distribution }
\description{
Compute a confidence interval for the mean of a response variable using the z-distribution.
}

\usage{
z.conf(x, sigma = NULL, conf = 0.95)
}

\arguments{
  \item{x}{a (non-empty) numeric vector of data values.}
  \item{sigma}{the population standard deviation. If \code{NULL}, the sample standard deviation is used. This is useful when the population standard deviation is known, otherwise it should be left as \code{NULL}.}
  \item{conf}{confidence level of the interval.}
}

\value{ 
A vector with two values: lower and upper confidence limits for the mean of the response variable.
}

\author{ Reza Mohammadi \email{a.mohammadi@uva.nl} }

\examples{
data(churn)

z.conf(x = churn$customer.calls, conf = 0.9)
}

\keyword{parameter learning}

