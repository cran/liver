\name{corona}
\alias{corona}
\docType{data}
\title{ Corona data set }

\description{ 
COVID-19 Coronavirus data - daily (up to 14 December 2020).   
}

\usage{ data( corona ) }

\format{ 
The \code{corona} dataset, as a data frame, contains \eqn{61900} rows and \eqn{12} columns (variables/features). 
}

\details{
This dataset can be downloaded from the UCI machine learning repository:

\url{https://data.europa.eu/euodp/en/data/dataset/covid-19-coronavirus-data}
}

\seealso{ \code{\link{churn}}, \code{\link{adult}}, \code{\link{risk}}, \code{\link{churnTel}}, \code{\link{bank}}, \code{\link{advertising}}, \code{\link{marketing}}, \code{\link{insurance}}, \code{\link{cereal}}, \code{\link{housePrice}}, \code{\link{house}} }

\examples{
data( corona )

str( corona )
}

\keyword{ datasets }
