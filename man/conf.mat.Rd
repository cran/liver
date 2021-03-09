\name{conf.mat}
\alias{conf.mat}

\title{	Confusion Matrix }
\description{
	Create a Confusion Matrix.
}
\usage{
conf.mat( pred, actual, cutoff = NULL, dnn = c( "Prediction", "Actual" ), ... )
}

\arguments{
    \item{pred  }{a vector of estimated values. }
    \item{actual}{a vector of actual values. }
    \item{cutoff}{cutoff value for the case that \code{pred} is vector of probabilites. }  
    \item{dnn   }{the names to be given to the dimensions in the result (the dimnames names). }
    \item{\dots }{options to be passed to \code{table}. }
}

\value{ the results of \code{table} on \code{pred} and \code{actual}. }

\author{ Reza Mohammadi \email{a.mohammadi@uva.nl} and Kevin Burke \email{kevin.burke@ul.ie} }

\examples{
pred   = c( "no", "yes", "yes", "no", "no", "yes", "no", "no" )
actual = c( "yes", "no", "yes", "no", "no", "no", "yes", "yes" )

conf.mat( pred, actual )
}

\keyword{parameter learning}

