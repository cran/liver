\name{zscore}
\alias{zscore}

\title{	Z-score scaling of numerical variables }
\description{
	Performs Z-score tranformation for numerical variables. 
}
\usage{
zscore(x, columns = NULL, na.rm = FALSE)
}

\arguments{
    \item{x}{a numerical \code{vector}, \code{matrix} or \code{data.frame}. }
    \item{columns}{ which columns are going to tranfer for the cases that \code{x} is a \code{matrix} or a \code{data.frame}. Defaults to all columns. }
    \item{na.rm}{a logical value indicating whether NA values in \code{x} should be stripped before the computation proceeds. }
}

\value{ transformed version of \code{x}. }

\author{ Reza Mohammadi \email{a.mohammadi@uva.nl} and Kevin Burke \email{kevin.burke@ul.ie} }

\seealso{ \code{\link[liver]{scaler}}, \code{\link[liver]{minmax}} }

\examples{
x = c(2.3, -1.4, 0, 3.45)

zscore(x)
}

\keyword{data preprocessing}

