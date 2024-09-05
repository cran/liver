\name{transform}
\alias{transform}

\title{	Z-score normalization }
\description{
	Performs variable transformation such as Z-score and min-max normalization. 
}
\usage{
transform( x, method = c( "minmax", "zscore" ), columns = NULL, na.rm = FALSE )
}

\arguments{
    \item{x}{a numerical \code{vector}, a \code{matrix} or a \code{data.frame}. }
    \item{method}{ a method to transfer \code{x}. }
    \item{columns}{ which columns are going to tranfer for the cases that \code{x} is a \code{matrix} or a \code{data.frame}. Defaults to all columns. }
    \item{na.rm}{a logical value indicating whether NA values in \code{x} should be stripped before the computation proceeds. }
}

\value{ transformed version of \code{x}. }

\author{ Reza Mohammadi \email{a.mohammadi@uva.nl} and Kevin Burke \email{kevin.burke@ul.ie} }

\seealso{ \code{\link[liver]{zscore}}, \code{\link[liver]{minmax}} }

\examples{
x = c( 2.3, -1.4, 0, 3.45 )

transform( x, method = "minmax" )

transform( x, method = "zscore" )

}

\keyword{data preprocessing}

