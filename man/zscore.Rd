\name{zscore}
\alias{zscore}

\title{	Z-score normalization }
\description{
	Performs Z-score normalization to transform numerical variables. 
}
\usage{
zscore( x, na.rm = FALSE )
}

\arguments{
    \item{x}{a numerical \code{vector}, \code{matrix} or \code{data.frame}. }
    \item{na.rm}{a logical value indicating whether NA values in \code{x} should be stripped before the computation proceeds. }
}

\value{ transformed version of \code{x}. }

\author{ Reza Mohammadi \email{a.mohammadi@uva.nl} and Kevin Burke \email{kevin.burke@ul.ie} }

\seealso{ \code{\link{transform}}, \code{\link{minmax}} }

\examples{
x = c( 2.3, -1.4, 0, 3.45 )

zscore( x )
}

\keyword{data preprocessing}

