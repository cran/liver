\name{kNN}
\alias{kNN}

\title{	k-Nearest Neighbour Classification }

\description{
\code{kNN} is used to perform k-nearest neighbour classification for test set using training set. For each row of the test set, the \code{k} nearest (based on Euclidean distance) training set vectors are found. then, the classification is done by majority vote (ties broken at random). This function provides a formula interface to the \code{\link[class:knn]{class::knn()}} function of \code{R} package \code{class}. In addition, it allows normalization of the given data using the \code{\link[liver]{scaler}} function.
}

\usage{
kNN(formula, train, test, k = 1, scaler = FALSE, type = "class", l = 0, 
    use.all = TRUE, na.rm = FALSE)
}

\arguments{
    \item{formula}{ a \link[stats]{formula}, with a response but no interaction terms. For the case of data frame, it is taken as the model frame (see \code{\link[stats]{model.frame})}. }
  \item{train}{ data frame or matrix of train set cases. }  
  \item{test}{ data frame or matrix of test set cases. }  
  \item{k}{ number of neighbours considered. }  

  \item{scaler}{a character with options \code{FALSE} (default), \code{"minmax"}, and \code{"zscore"}. 
  Option \code{"minmax"} means no transformation. This option allows the users to use normalized version of the train and test sets for the kNN aglorithm. 
  }  

  \item{type}{either \code{"class"} (default) for the predicted class or \code{"prob"} for model confidence values.}
  
  \item{l}{ minimum vote for definite decision, otherwise \code{doubt}. (More precisely, less than \code{k-l} dissenting votes are allowed, even if \code{k} is increased by ties.) } 

  \item{use.all}{ controls handling of ties. If true, all distances equal to the \code{k}th largest are included. If false, a random selection of distances equal to the \code{k}th is chosen to use exactly \code{k} neighbours.
 }  
  \item{na.rm}{a logical value indicating whether NA values in \code{x} should be stripped before the computation proceeds. }
}

\value{ 
When \code{type = "class"} (default), a factor vector is returned, 
in which the \code{doubt} will be returned as \code{NA}.
When \code{type = "prob"}, a matrix of confidence values is returned
(one column per class).
 }

\references{
  Ripley, B. D. (1996) \emph{Pattern Recognition and Neural Networks.} Cambridge.

  Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied Statistics with S.} Fourth edition. Springer.
}

\author{ Reza Mohammadi \email{a.mohammadi@uva.nl} and Kevin Burke \email{kevin.burke@ul.ie} }

\seealso{ \code{\link[liver]{kNN}}, \code{\link[liver]{scaler}} }

\examples{
data(risk)

train = risk[1:100, ]
test  = risk[  101, ]

kNN(risk ~ income + age, train = train, test = test)
}

\keyword{models}

