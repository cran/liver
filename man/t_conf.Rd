\name{t_conf}
\alias{t_conf}

\title{ Confdidence interval for mean }
\description{
Compute a confidence interval for the mean of a response variable using the t-distribution.
}

\usage{
t_conf(x, conf = 0.95, ...)
}

\arguments{
    \item{x}{a (non-empty) numeric vector of data values.}
  \item{conf}{confidence level of the interval.}
  \item{\dots}{further arguments to be passed to \code{t.test}. }  
}

\value{ 
A vector with two values: lower and upper confidence limits for the mean of the response variable.
}

\author{ Reza Mohammadi \email{a.mohammadi@uva.nl} }

\examples{
data(churn)

t_conf(churn$customer.calls, conf = 0.9)
}

\keyword{parameter learning}

