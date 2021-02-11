\name{house}
\alias{house}
\docType{data}
\title{ house data set }

\description{ 
The house dataset contains \eqn{6} features and \eqn{414} records. The target feature is \emph{unit.price} and the remaining 5 variables are predictors.
}

\usage{ data( house ) }

\format{ 

The \code{house} dataset, as a data frame, contains \eqn{414} rows and \eqn{6} columns (variables/features). The \eqn{6} variables are:
 \itemize{
\item \code{house.age}: house age (numeric, in year).
\item \code{distance.to.MRT}: distance to the nearest MRT station (numeric).
\item \code{stores.number}: number of convenience stores (numeric).
\item \code{latitude}: latitude (numeric).
\item \code{longitude}: longitude (numeric).
\item \code{unit.price}: house price of unit area (numeric).
  }
}

\details{
A detailed description of the dataset can be found:

\url{https://www.kaggle.com/quantbruce/real-estate-price-prediction}
}

\seealso{ \code{\link{adult}}, \code{\link{risk}}, \code{\link{churn}}, \code{\link{churnTel}}, \code{\link{bank}}, \code{\link{advertising}}, \code{\link{marketing}}, \code{\link{insurance}}, \code{\link{cereal}}, \code{\link{housePrice}} }

\examples{
data( house )

str( house )
}

\keyword{ datasets }
