\name{housePrice}
\alias{housePrice}

\docType{data}

\title{ housePrice dataset }

\description{ 
This data set contains \eqn{1460} rows and \eqn{81} columns (features). the "SalePrice" column is the target.
}

\usage{ data(housePrice) }

\format{ 
the \code{housePrice} dataset, as a data frame, contains \eqn{1460} rows and \eqn{81} columns (variables/features). 
}

\details{
For more information related to the dataset see: \cr
\url{https://www.kaggle.com/datasets/lespin/house-prices-dataset}
}

\source{
This dataset comes from: \cr
\url{https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques}
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
          \code{\link[liver]{house}}, 
          \code{\link[liver]{redWines}}, 
          \code{\link[liver]{whiteWines}}, 
          \code{\link[liver]{insurance}}, 
          \code{\link[liver]{caravan}}, 
          \code{\link[liver]{fertilizer}}, 
          \code{\link[liver]{corona}}
          }

\examples{
data(housePrice)
str(housePrice)
}

\keyword{ datasets }
