\name{zscore}
\alias{zscore}

\title{	Z-score scaling of numerical variables }
\description{
	Performs Z-score tranformation for numerical variables. 
}
\usage{
zscore(x, col = "auto", mean = NULL, sd = NULL, na.rm = FALSE)
}

\arguments{
    \item{x}{a numerical \code{vector}, \code{matrix} or \code{data.frame}. }
    \item{col}{ a character vector of column names or indices. If \code{"auto"}, all numeric columns will be transformed. If \code{"all"}, all columns will be transformed. }
    \item{mean}{ a numerical value or vector indicating the \code{mean} to use for Z-score calculation; if NULL, the default is the mean of \code{x}. }
    \item{sd}{ a numerical value or vector indicating the standard deviation(s) to use for Z-score calculation; if NULL, the default is the standard deviation of \code{x}. }
    \item{na.rm}{a logical value indicating whether NA values in \code{x} should be stripped before the computation proceeds. }
}

\value{ transformed version of \code{x}. }

\author{ Reza Mohammadi \email{a.mohammadi@uva.nl} and Kevin Burke \email{kevin.burke@ul.ie} }

\seealso{ \code{\link[liver]{scaler}}, \code{\link[liver]{minmax}} }

\examples{
x = c(2.3, -1.4, 0, 3.45)

zscore(x)
zscore(x, mean = 1, sd = 2)
}

\keyword{data preprocessing}

