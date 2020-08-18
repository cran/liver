\name{classifyRisk}
\alias{classifyRisk}
\docType{data}
\title{ classifyRisk data set }

\description{ 
The classifyRisk dataset containing 6 features and 246 records. The target feature is \emph{risk}, a factor with levels "good risk" and "bad loss" along with 5 predictors. 
}

\usage{ data( classifyRisk ) }

\format{ 

The \code{classifyRisk} dataset, as a data frame, contains \eqn{246} rows (customers) and \eqn{6} columns (variables/features). The \eqn{6} variables are:
 \itemize{

\item \code{mortgage}: A factor with levels "n" and "y".
\item \code{nr_loans}: Number of loans that constomers have.
\item \code{age}: age in years.
\item \code{marital_status}: A factor with levels "single", "married", and "other".
\item \code{income}: yearly income.
\item \code{risk}: A factor with levels "good risk" and "bad loss".
  }
}

\references{
Larose, D. T. and Larose, C. D. (2014). Discovering knowledge in data: an introduction to data mining. \emph{John Wiley & Sons}.
}

\seealso{ \code{\link{adult}}, \code{\link{churn}}, \code{\link{churnTel}}, \code{\link{housePrice}} }

\examples{
data( classifyRisk )

str( classifyRisk )
}

\keyword{ datasets }
