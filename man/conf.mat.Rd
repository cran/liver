\name{conf.mat}
\alias{conf.mat}

\title{	Confusion Matrix }
\description{ Create a Confusion Matrix. }

\usage{
conf.mat( pred, actual, cutoff = NULL, reference = NULL, 
          proportion = FALSE, dnn = c( "Predict", "Actual" ), ... )
}

\arguments{
    \item{pred  }{a vector of estimated values. }
    \item{actual}{a vector of actual values. }
    \item{cutoff}{cutoff value for the case that \code{pred} is vector of probabilites. }  
    \item{reference}{a factor of classes to be used as the true results. }
	\item{proportion}{
		Logical: FALSE (default) for a confusion matrix with number of cases. 
		         TRUE, for a confusion matrix with the proportion of cases.
	}
    \item{dnn   }{the names to be given to the dimensions in the result (the dimnames names). }
    \item{\dots }{options to be passed to \code{table}. }
}

\value{ the results of \code{table} on \code{pred} and \code{actual}. }

\author{ Reza Mohammadi \email{a.mohammadi@uva.nl} and Kevin Burke \email{kevin.burke@ul.ie} }

\seealso{ \code{\link[liver]{conf.mat.plot}}, \code{\link[liver]{accuracy}} }

\examples{
pred   = c( "no", "yes", "yes", "no", "no", "yes", "no", "no" )
actual = c( "yes", "no", "yes", "no", "no", "no", "yes", "yes" )

conf.mat( pred, actual )
conf.mat( pred, actual, proportion = TRUE )
}

\keyword{parameter learning}

