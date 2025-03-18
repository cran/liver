\name{minmax}
\alias{minmax}

\title{	Min-Max scaling of numerical variables }
\description{
	Performs Min-Max tranformation for numerical variables. 
}
\usage{
minmax(x, col = "auto", min = NULL, max = NULL, na.rm = FALSE)
}

\arguments{
    \item{x}{a numerical \code{vector}, \code{matrix} or \code{data.frame}. }
    \item{col}{ a character vector of column names or indices. If \code{"auto"}, all numeric columns will be transformed. If \code{"all"}, all columns will be transformed. }
    \item{min}{ a numerical value or vector indicating the minimum value(s) to use for Min-Max tranformation; if NULL, the default is based on \code{x}. }
    \item{max}{ a numerical value or vector indicating the maximum value(s) to use for Min-Max tranformation; if NULL, the default is based on \code{x}. }
    \item{na.rm}{a logical value indicating whether NA values in \code{x} should be stripped before the computation proceeds. }
}

\value{ transformed version of \code{x}. }

\author{ Reza Mohammadi \email{a.mohammadi@uva.nl} and Kevin Burke \email{kevin.burke@ul.ie} }

\seealso{ \code{\link[liver]{scaler}}, \code{\link[liver]{zscore}} }

\examples{
x = c(2.3, -1.4, 0, 3.45)

minmax(x)

minmax(x, min = 0, max = 1)
}

\keyword{data preprocessing}

