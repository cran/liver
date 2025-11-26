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
\url{https://archive.ics.uci.edu/dataset/477/real+estate+valuation+data+set} \cr
\url{https://www.kaggle.com/quantbruce/real-estate-price-prediction}
}

\source{
This dataset originally comes from the UCI repository of machine learning databases: \cr
\url{https://archive.ics.uci.edu/dataset/477/real+estate+valuation+data+set}
}

\references{
Reza Mohammadi (2025). Data Science Foundations and Machine Learning with R: From Data to Decisions. \url{https://book-data-science-r.netlify.app}.
}

\seealso{ \code{\link[liver]{bank}}, 
          \code{\link[liver]{churn}}, 
          \code{\link[liver]{churnCredit}}, 
          \code{\link[liver]{churnTel}}, 
          \code{\link[liver]{adult}}, 
          \code{\link[liver]{risk}}, 
          \code{\link[liver]{cereal}}, 
          \code{\link[liver]{advertising}}, 
          \code{\link[liver]{marketing}}, 
          \code{\link[liver]{drug}}, 
          \code{\link[liver]{housePrice}}, 
          \code{\link[liver]{redWines}}, 
          \code{\link[liver]{whiteWines}}, 
          \code{\link[liver]{insurance}}, 
          \code{\link[liver]{caravan}}, 
          \code{\link[liver]{fertilizer}}, 
          \code{\link[liver]{corona}}
          }

\examples{
data(house)
str(house)
}

\keyword{ datasets }
