\name{corona}
\alias{corona}
\docType{data}
\title{ Corona data set }

\description{ 
COVID-19 Coronavirus data - daily (up to 14 December 2020).   
}

\usage{ data(corona) }

\format{ 
the \code{corona} dataset, as a data frame, contains \eqn{61900} rows and \eqn{12} columns (variables/features). 
}

\details{
This dataset can be downloaded from the UCI machine learning repository:

\url{https://data.europa.eu/euodp/en/data/dataset/covid-19-coronavirus-data}
}

\seealso{ \code{\link[liver]{churn}}, \code{\link[liver]{adult}}, \code{\link[liver]{risk}}, \code{\link[liver]{churnTel}}, \code{\link[liver]{bank}}, \code{\link[liver]{advertising}}, \code{\link[liver]{marketing}}, \code{\link[liver]{insurance}}, \code{\link[liver]{cereal}}, \code{\link[liver]{housePrice}}, \code{\link[liver]{house}} }

\examples{
data(corona)

str(corona)
}

\keyword{ datasets }
