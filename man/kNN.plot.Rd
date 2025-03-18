\name{kNN.plot}
\alias{kNN.plot}

\title{ Visualizing the Optimal Number of k }
\description{
Visualizing the Optimal Number of k for k-Nearest Neighbour (\code{kNN}) algorithm based on accuracy or Mean Square Error (MSE). 
}
\usage{
kNN.plot(formula, train, test, k.max = 10, scaler = FALSE, 
         base = "accuracy", reference = NULL, cutoff = NULL, 
         type = "class", report = FALSE, set.seed = NULL, ...)
}

\arguments{
    \item{formula}{ a \link[stats]{formula}, with a response but no interaction terms. For the case of data frame, it is taken as the model frame (see \code{\link[stats]{model.frame})}. }
  \item{train}{ data frame or matrix of train set cases. }  
  \item{test}{ data frame or matrix of test set cases. }  
  \item{k.max}{ the maximum number of neighbors to consider can either be a single value, with a minimum of 2, or a vector representing a range of values k.}  

  \item{scaler}{a character with options \code{FALSE} (default), \code{"minmax"}, and \code{"zscore"}. 
  Option \code{"minmax"} means no transformation. This option allows the users to use normalized version of the train and test sets for the kNN aglorithm. 
  }  
  \item{base}{ base measurement: \code{accuracy} (default), \code{error}, or \code{MSE} for Mean Square Error.  }  
  \item{reference}{a factor of classes to be used as the true results. }
  \item{cutoff}{cutoff value for the case that the output of knn algorithm is vector of probabilites. }  
  \item{type}{either \code{"class"} (default) for the predicted class or \code{"prob"} for model confidence values.}

  \item{report}{a character with options \code{FALSE} (default) and \code{TRUE}. 
  Option \code{TRUE} reports the values of the \code{base} measurement. 
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

\seealso{ \code{\link[liver]{kNN}}, \code{\link[liver]{scaler}} }

\examples{
data(risk)

partition_risk <- partition(data = risk, ratio = c(0.6, 0.4))

train <- partition_risk$part1
test  <- partition_risk$part1

kNN.plot(risk ~ income + age, train = train, test = test)
kNN.plot(risk ~ income + age, train = train, test = test, base = "error")
}

\keyword{models}

