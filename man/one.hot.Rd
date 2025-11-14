\name{one.hot}
\alias{one.hot}

\title{	One Hot Encoder }
\description{
	One-Hot-Encode unordered factor columns of a data.frame, matrix, or data.table, using the
	\code{\link[mltools:one_hot]{mltools::one_hot()}} mltools::one_hot function.
}
\usage{
one.hot(data, cols = "auto", sparsifyNAs = FALSE, naCols = FALSE, 
                   dropCols = FALSE, dropUnusedLevels = FALSE)
}

\arguments{
    \item{data}{a numerical \code{vector}, \code{matrix}, \code{data.frame}, or \code{data.table}. }
    \item{cols}{a character vector of column names or indices to one-hot-encode. If \code{"auto"}, all unordered factor columns will be one-hot-encoded. }
    \item{sparsifyNAs}{a logical value indicating whether to converte NAs to 0s. }
    \item{naCols}{a logical value indicating whether to create a separate column for NAs. }
    \item{dropCols}{a logical value indicating whether to drop the original columns which are one-hot-encoded.}
    \item{dropUnusedLevels}{a logical value indicating whether to drop unused factor levels. }
}

\author{ Reza Mohammadi \email{a.mohammadi@uva.nl} and Kevin Burke \email{kevin.burke@ul.ie} }

\seealso{ \code{\link[liver]{scaler}} }

\examples{
data(risk)
str(risk)

risk_one_hot <- one.hot(risk, cols = "auto")
str(risk_one_hot)
}

\keyword{data preprocessing}

