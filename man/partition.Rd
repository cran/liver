\name{partition}
\alias{partition}

\title{	Partition the data }
\description{
	Randomly partitions the data (primarly intended to split into "training" and "test" sets) according to the supplied probabilities. 
}
\usage{
partition( data, prob = c( 0.7, 0.3 ) )
}

\arguments{
    \item{data}{an (\eqn{n \times p}) \code{matrix} or a \code{data.frame}. }
    \item{prob}{ a numerical vector in [0, 1]. }
}

\value{ 
a list which includes the data partitions.
}

\author{ Reza Mohammadi \email{a.mohammadi@uva.nl} and Kevin Burke \email{kevin.burke@ul.ie} }

\examples{
data( iris )

partition( data = iris, prob = c( 0.7, 0.3 ) )
}

\keyword{data preprocessing}

