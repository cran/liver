\name{accuracy}
\alias{accuracy}

\title{	Average classification accuracy }
\description{
	Computes average classification accuracy. 
}
\usage{
accuracy( pred, actual, cutoff = NULL, reference = NULL )
}

\arguments{
    \item{pred}{a numerical vector of estimated values. }
    \item{actual}{a numerical vector of actual values. }
    \item{cutoff}{cutoff value for the case that \code{pred} is vector of probabilites. }  
    \item{reference}{a factor of classes to be used as the true results. }
. }

\value{ The computed average classification accuracy (numeric value). }

\author{ Reza Mohammadi \email{a.mohammadi@uva.nl} and Kevin Burke \email{kevin.burke@ul.ie} }

\seealso{ \code{\link{conf.mat}}, \code{\link{mse}}, \code{\link{mae}} }

\examples{
pred   = c( "no", "yes", "yes", "no", "no", "yes", "no", "no" )
actual = c( "yes", "no", "yes", "no", "no", "no", "yes", "yes" )

accuracy( pred, actual )
}

\keyword{parameter learning}

