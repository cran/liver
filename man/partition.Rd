\name{partition}
\alias{partition}

\title{	Partition the data }
\description{
	Randomly partitions the data (primarly intended to split into "training" and "test" sets) according to the supplied probabilities. 
}
\usage{
partition(data, ratio = c(0.7, 0.3), set.seed = NULL)
}

\arguments{
    \item{data}{an (\eqn{n \times p}) \code{matrix} or a \code{data.frame}. }
    \item{ratio}{ a numerical vector in range of [0, 1]. }
    \item{set.seed}{a single value, interpreted as an integer, or NULL. }
}

\value{ 
a list which includes the data partitions.
}

\author{ Reza Mohammadi \email{a.mohammadi@uva.nl} and Kevin Burke \email{kevin.burke@ul.ie} }

\examples{
data(iris)

partition(data = iris, ratio = c(0.7, 0.3))
}

\keyword{data preprocessing}

