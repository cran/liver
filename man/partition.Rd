\name{partition}
\alias{partition}

\title{Partition Data}

\description{
Randomly partitions data into two or more subsets according to the supplied
ratios. Optionally, stratified partitioning can be performed using a categorical
or numerical variable.
}

\usage{
partition(data, ratio = c(0.7, 0.3), strata = NULL, set.seed = NULL)
}

\arguments{
\item{data}{An (\eqn{n \times p}) \code{matrix} or \code{data.frame}.}

\item{ratio}{A numeric vector specifying the proportions of observations assigned to the partitions.}

\item{strata}{Optional variable used for stratified partitioning. For categorical variables, the proportions of the levels are approximately preserved across partitions. For numerical variables, observations are
distributed across partitions according to their ordered values.}

\item{set.seed}{A single value, interpreted as an integer, or \code{NULL}.}

}

\value{
A list containing the data partitions.
}

\author{ Reza Mohammadi \email{a.mohammadi@uva.nl} }

\examples{
data(iris)

parts <- partition(iris, ratio = c(0.7, 0.3), set.seed = 42)
sapply(parts, nrow)
}

\keyword{data preprocessing}

