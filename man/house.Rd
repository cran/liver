\name{house}
\alias{house}
\docType{data}
\title{ house data set }

\description{ 
the house dataset contains \eqn{6} features and \eqn{414} records. the target feature is \emph{unit.price} and the remaining 5 variables are predictors.
}

\usage{ data(house) }

\format{ 

the \code{house} dataset, as a data frame, contains \eqn{414} rows and \eqn{6} columns (variables/features). the \eqn{6} variables are:
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
For more information related to the dataset see: \cr
\url{https://www.kaggle.com/quantbruce/real-estate-price-prediction}
}

\seealso{ \code{\link[liver]{adult}}, \code{\link[liver]{risk}}, \code{\link[liver]{churn}}, \code{\link[liver]{churnTel}}, \code{\link[liver]{bank}}, \code{\link[liver]{advertising}}, \code{\link[liver]{marketing}}, \code{\link[liver]{insurance}}, \code{\link[liver]{cereal}}, \code{\link[liver]{housePrice}} }

\examples{
data(house)
str(house)
}

\keyword{ datasets }
