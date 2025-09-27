\name{risk}
\alias{risk}
\docType{data}
\title{ Risk data set }

\description{ 
the \emph{risk} dataset containing 6 features and 246 records. the target feature is \emph{risk}, a factor with levels "good risk" and "bad risk" along with 5 predictors. 
}

\usage{ data(risk) }

\format{ 

the \code{risk} dataset, as a data frame, contains \eqn{246} rows (customers) and \eqn{6} columns (variables/features). the \eqn{6} variables are:
 \itemize{

\item \code{age}: age in years.
\item \code{marital}: A factor with levels "single", "married", and "other".
\item \code{income}: yearly income.
\item \code{mortgage}: A factor with levels "yes" and "no".
\item \code{nr_loans}: Number of loans that constomers have.
\item \code{risk}: A factor with levels "good risk" and "bad risk".
  }
}

\references{
Larose, D. T. and Larose, C. D. (2014). Discovering knowledge in data: an introduction to data mining. \emph{John Wiley & Sons}.
}

\seealso{ \code{\link[liver]{adult}}, \code{\link[liver]{churn}}, \code{\link[liver]{churnTel}}, \code{\link[liver]{bank}}, \code{\link[liver]{advertising}}, \code{\link[liver]{marketing}}, \code{\link[liver]{insurance}}, \code{\link[liver]{cereal}}, \code{\link[liver]{housePrice}}, \code{\link[liver]{house}} }

\examples{
data(risk)
str(risk)
}

\keyword{ datasets }
