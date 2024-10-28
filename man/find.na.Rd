\name{find.na}
\alias{find.na}

\title{	find.na }

\description{
	Finding missing values.
}

\usage{
find.na(x)
}

\arguments{
    \item{x}{a numerical \code{vector}, \code{matrix} or \code{data.frame}. }
}

\value{ A numeric matrix with two columns. }

\author{ Reza Mohammadi \email{a.mohammadi@uva.nl} and Kevin Burke \email{kevin.burke@ul.ie} }

\examples{
x = c(2.3, NA, -1.4, 0, 3.45)

find.na(x)
}

\keyword{data preprocessing}

