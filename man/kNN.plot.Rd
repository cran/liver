\name{kNN.plot}
\alias{kNN.plot}

\title{ Visualizing the Optimal Number of k }
\description{
Visualizing the Optimal Number of k for k-Nearest Neighbour Classification\code{kNN} based on Mean Square Error (MSE). 
}
\usage{
kNN.plot( formula, train, test, k.max = 10, transform = FALSE, set.seed = NULL, ... )
}

\arguments{
    \item{formula}{ a \link{formula}, with a response but no interaction terms. For the case of data frame, it is taken as the model frame (see \code{\link{model.frame})}. }
  \item{train}{ data frame or matrix of train set cases. }  
  \item{test}{ data frame or matrix of test set cases. }  
  \item{k.max}{ the maximum number of number of neighbours to consider, must be at least two. }  

  \item{transform}{a character with options \code{FALSE} (default), \code{"minmax"}, and \code{"zscore"}. 
  Option \code{"minmax"} means no transformation. This option allows the users to use normalized version of the train and test sets for the kNN aglorithm. 
  }  
  
  \item{set.seed}{a single value, interpreted as an integer, or NULL. }
  \item{ \dots }{options to be passed to \code{kNN()}. }
}

\references{
  Ripley, B. D. (1996)
  \emph{Pattern Recognition and Neural Networks.} Cambridge.

  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}

\author{ Reza Mohammadi \email{a.mohammadi@uva.nl} and Kevin Burke \email{kevin.burke@ul.ie} }

\seealso{ \code{\link{kNN}}, \code{\link{transform}} }

\examples{
data( risk )

train = risk[ 1:150, ]
test  = risk[ 150:246, ]

kNN.plot( risk ~ income + age, train = train, test = test )
}

\keyword{models}

