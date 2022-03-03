\name{skewness}
\alias{skewness}

\title{	Skewness }

\description{
	Computes the skewness for each field. 
}

\usage{
skewness( x, na.rm = FALSE )
}

\arguments{
    \item{x}{a numerical \code{vector}, \code{matrix} or \code{data.frame}. }
    \item{na.rm}{a logical value indicating whether NA values in \code{x} should be stripped before the computation proceeds. }
}

\value{ A numeric vector of skewness values. }

\author{ Reza Mohammadi \email{a.mohammadi@uva.nl} and Kevin Burke \email{kevin.burke@ul.ie} }

\examples{
x = c( 2.3, -1.4, 0, 3.45 )

skewness( x )
}

\keyword{data preprocessing}
\keyword{parameter learning}

