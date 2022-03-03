\name{risk}
\alias{risk}
\docType{data}
\title{ Risk data set }

\description{ 
The \emph{risk} dataset containing 6 features and 246 records. The target feature is \emph{risk}, a factor with levels "good risk" and "bad risk" along with 5 predictors. 
}

\usage{ data( risk ) }

\format{ 

The \code{risk} dataset, as a data frame, contains \eqn{246} rows (customers) and \eqn{6} columns (variables/features). The \eqn{6} variables are:
 \itemize{

\item \code{age}: age in years.
\item \code{marital}: A factor with levels "single", "married", and "other".
\item \code{income}: yearly income.
\item \code{mortgage}: A factor with levels "yes" and "no".
\item \code{nr_loans}: Number of loans that constomers have.
\item \code{risk}: A factor with levels "good risk" and "bad risk".
  }
}

\seealso{ \code{\link{adult}}, \code{\link{churn}}, \code{\link{churnTel}}, \code{\link{bank}}, \code{\link{advertising}}, \code{\link{marketing}}, \code{\link{insurance}}, \code{\link{cereal}}, \code{\link{housePrice}}, \code{\link{house}} }

\examples{
data( risk )

str( risk )
}

\keyword{ datasets }
