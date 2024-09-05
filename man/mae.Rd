\name{mae}
\alias{mae}

\title{	Mean Absolute Error (MAE) }
\description{
	Computes mean absolute error. 
}
\usage{
mae( pred, actual, weight = 1, na.rm = FALSE )
}

\arguments{
    \item{pred}{a numerical vector of estimated values. }
    \item{actual}{a numerical vector of actual values. }
    \item{weight}{a numerical vector of weights the same length as \code{pred}. }
    \item{na.rm}{a logical value indicating whether NA values in \code{pred} should be stripped before the computation proceeds. }
}

\value{ the computed mean squared error (numeric value). }

\author{ Reza Mohammadi \email{a.mohammadi@uva.nl} and Kevin Burke \email{kevin.burke@ul.ie} }

\seealso{ \code{\link[liver]{mse}} }

\examples{
pred   = c( 2.3, -1.4, 0, 3.45 )
actual = c( 2.1, -0.9, 0, 2.99 )
  
mae( pred, actual )
}

\keyword{parameter learning}

