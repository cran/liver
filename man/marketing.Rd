\name{marketing}
\alias{marketing}
\docType{data}
\title{ marketing data set }

\description{ 
The marketing dataset contains \eqn{8} features and \eqn{40} records as 40 days that report how much we spent, how many clicks, impressions and transactions we got, whether or not a display campaign was running, as well as our revenue, click-through-rate and conversion rate. The target feature is \emph{revenue} and the remaining 7 variables are predictors.
}

\usage{ data( marketing ) }

\format{ 
The \code{marketing} dataset, as a data frame, contains \eqn{40} rows and \eqn{8} columns (variables/features). The \eqn{8} variables are:
 \itemize{

\item \code{spend}: daily send of money on PPC (apy-per-click).
\item \code{clicks}: number of clicks on for that ad.
\item \code{impressions}: amount of impressions per day.
\item \code{display}: whether or not a display campaign was running.
\item \code{transactions}: number of transactions per day.
\item \code{click.rate}:  click-through-rate.
\item \code{conversion.rate}: conversion rate.
\item \code{revenue}: daily revenue.
  }
}

\details{
A detailed description of the dataset can be found:

\url{https://github.com/chrisBow/marketing-regression-part-one}
}

\seealso{ \code{\link{adult}}, \code{\link{risk}}, \code{\link{churn}}, \code{\link{churnTel}}, \code{\link{bank}}, \code{\link{advertising}}, \code{\link{insurance}}, \code{\link{cereal}}, \code{\link{housePrice}}, \code{\link{house}} }

\examples{
data( marketing )

str( marketing )
}

\keyword{ datasets }
