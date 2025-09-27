\name{redWines}
\alias{redWines}
\docType{data}
\title{ Red wines data set }

\description{ 
the redWines datasets are related to red variants of the Portuguese "Vinho Verde" wine. Due to privacy and logistic issues, only physicochemical (inputs) and sensory (the output) variables are available (e.g. there is no data about grape types, wine brand, wine selling price, etc.).

the dataset can be viewed as classification or regression tasks. the classes are ordered and not balanced (e.g. there are many more normal wines than excellent or poor ones). Outlier detection algorithms could be used to detect the few excellent or poor wines. Also, we are not sure if all input variables are relevant. So it could be interesting to test feature selection methods.
}

\usage{ data(redWines) }

\format{ 

the \code{redWines} dataset, as a data frame, contains \eqn{1599} rows and \eqn{12} columns (variables/features). the \eqn{12} 
variables are:
 \itemize{
Input variables (based on physicochemical tests):

\item \code{fixed acidity}
\item \code{volatile acidity}
\item \code{citric acid}
\item \code{residual sugar}
\item \code{chlorides}
\item \code{free sulfur dioxide}
\item \code{total sulfur dioxide}
\item \code{density}
\item \code{pH}
\item \code{sulphates}
\item \code{alcohol}

Output variable (based on sensory data)

\item \code{quality}: score between 0 and 10.
  }
}

\details{
For more information related to the dataset see the UCI Machine Learning Repository: \cr
\url{https://archive.ics.uci.edu/dataset/186/wine+quality}
}

\references{
Cortez, P., Cerdeira, A., Almeida, F., Matos, T., and Reis, J. (2009). Modeling wine preferences by data mining from physicochemical properties. \emph{Decision support systems}, 47(4), 547-553.
}

\seealso{ \code{\link[liver]{whiteWines}}, \code{\link[liver]{adult}}, \code{\link[liver]{risk}}, \code{\link[liver]{churn}}, \code{\link[liver]{churnTel}}, \code{\link[liver]{bank}}, \code{\link[liver]{advertising}}, \code{\link[liver]{marketing}}, \code{\link[liver]{insurance}}, \code{\link[liver]{cereal}}, \code{\link[liver]{housePrice}}, \code{\link[liver]{house}} }

\examples{
data(redWines)
str(redWines)
}

\keyword{ datasets }
