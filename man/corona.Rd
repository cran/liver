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

\source{
The original source can be found: \cr
\url{https://data.europa.eu/data/datasets/covid-19-coronavirus-data?locale=en}
}

\seealso{ \code{\link[liver]{bank}}, 
          \code{\link[liver]{churn_mlc}}, 
          \code{\link[liver]{churn}}, 
          \code{\link[liver]{churn_tel}}, 
          \code{\link[liver]{adult}}, 
          \code{\link[liver]{risk}}, 
          \code{\link[liver]{cereal}}, 
          \code{\link[liver]{advertising}}, 
          \code{\link[liver]{marketing}}, 
          \code{\link[liver]{drug}}, 
          \code{\link[liver]{house}}, 
          \code{\link[liver]{house_price}}, 
          \code{\link[liver]{red_wines}}, 
          \code{\link[liver]{white_wines}}, 
          \code{\link[liver]{insurance}}, 
          \code{\link[liver]{caravan}}, 
          \code{\link[liver]{fertilizer}}
          }

\examples{
data(corona)
str(corona)
}

\keyword{ datasets }
