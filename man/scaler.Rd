\name{scaler}
\alias{scaler}

\title{	Feature scaling }
\description{
	Performs feature scaling such as Z-score and min-max scaling. 
}
\usage{
scaler(x, scale = c("minmax", "zscore"), col = "auto", 
       par1 = NULL, par2 = NULL, na.rm = FALSE)
}

\arguments{
    \item{x}{a numerical \code{vector}, a \code{matrix} or a \code{data.frame}. }
    \item{scale}{ a transfer for \code{x}. }
    \item{col}{ a character vector of column names or indices. If \code{"auto"}, all numeric columns will be transformed. If \code{"all"}, all columns will be transformed. }
    \item{par1}{ a numerical value or vector that for the case \code{scale = "minmax"} indicating the maximum value(s) and for the case \code{scale = "zscore"} indicating the mean value(s). }
    \item{par2}{ a numerical value or vector that for the case \code{scale = "minmax"} indicating the maximum value(s) and for the case \code{scale = "zscore"} indicating the sd value(s). }
    \item{na.rm}{a logical value indicating whether NA values in \code{x} should be stripped before the computation proceeds. }
}

\value{ transformed version of \code{x}. }

\author{ Reza Mohammadi \email{a.mohammadi@uva.nl} and Kevin Burke \email{kevin.burke@ul.ie} }

\seealso{ \code{\link[liver]{zscore}}, \code{\link[liver]{minmax}} }

\examples{
x = c(2.3, -1.4, 0, 3.45)

scaler(x, scale = "minmax")

scaler(x, scale = "zscore")
}

\keyword{data preprocessing}

