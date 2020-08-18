\name{minmax}
\alias{minmax}

\title{	Min-Max normalization }
\description{
	Performs Min-Max normalization of numerical variables. 
}
\usage{
minmax( x, na.rm = FALSE )
}

\arguments{
    \item{x}{a numerical \code{vector}, \code{matrix} or \code{data.frame}. }
    \item{na.rm}{a logical value indicating whether NA values in \code{x} should be stripped before the computation proceeds. }
}

\value{ transformed version of \code{x}. }

\author{ Reza Mohammadi \email{a.mohammadi@uva.nl} and Kevin Burke \email{kevin.burke@ul.ie} }

\seealso{ \code{\link{transform}}, \code{\link{zscore}} }

\examples{
x = c( 2.3, -1.4, 0, 3.45 )

minmax( x )
}

\keyword{data preprocessing}

