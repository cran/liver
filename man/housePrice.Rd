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
For more information related to the dataset see:

\verb{https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data}
}

\seealso{ \code{\link[liver]{adult}}, \code{\link[liver]{risk}}, \code{\link[liver]{churn}}, \code{\link[liver]{churnTel}}, \code{\link[liver]{bank}}, \code{\link[liver]{advertising}}, \code{\link[liver]{marketing}}, \code{\link[liver]{insurance}}, \code{\link[liver]{cereal}}, \code{\link[liver]{house}} }

\examples{
data(housePrice)

str(housePrice)
}

\keyword{ datasets }
