\name{prop.conf}
\alias{prop.conf}

\title{ Confdidence interval for proportion }

\description{
Compute a confidence interval for the proportion of a response variable using the normal distribution.
}

\usage{
prop.conf(x, n, conf = 0.95, ...)
}

\arguments{
  \item{x}{a vector of counts of successes, a one-dimensional table with two entries, or a two-dimensional table (or matrix) with 2 columns, giving the counts of successes and failures, respectively.}
  \item{n}{a vector of counts of trials; ignored if \code{x} is a matrix or a table.}
  \item{conf}{confidence level of the interval.}
  \item{\dots}{further arguments to be passed to \code{prop.test}. }  
}

\value{ 
A vector with two values: lower and upper confidence limits for the proportion of the response variable.
}

\author{ Reza Mohammadi \email{a.mohammadi@uva.nl} }

\examples{
data(churn)

prop.conf(table(churn$churn), conf = 0.9)
}

\keyword{parameter learning}

