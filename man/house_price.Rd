\name{house_price}
\alias{house_price}

\docType{data}

\title{ house_price dataset }

\description{ 
This data set contains \eqn{1460} rows and \eqn{81} columns (features). the "SalePrice" column is the target.
}

\usage{ data(house_price) }

\format{ 
the \code{house_price} dataset, as a data frame, contains \eqn{1460} rows and \eqn{81} columns (variables/features). 
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

\seealso{ \code{\link[liver]{house}}}

\examples{
data(house_price)
str(house_price)
}

\keyword{ datasets }
