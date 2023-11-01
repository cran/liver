\name{whiteWines}
\alias{whiteWines}
\docType{data}
\title{ White wines data set }

\description{ 
The whiteWines datasets are related to white variants of the Portuguese "Vinho Verde" wine. Due to privacy and logistic issues, only physicochemical (inputs) and sensory (the output) variables are available (e.g. there is no data about grape types, wine brand, wine selling price, etc.).

The dataset can be viewed as classification or regression tasks. The classes are ordered and not balanced (e.g. there are many more normal wines than excellent or poor ones). Outlier detection algorithms could be used to detect the few excellent or poor wines. Also, we are not sure if all input variables are relevant. So it could be interesting to test feature selection methods.
}

\usage{ data( whiteWines ) }

\format{ 

The \code{whiteWines} dataset, as a data frame, contains \eqn{4898} rows and \eqn{12} columns (variables/features). The \eqn{12} 
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
This dataset can be downloaded from the UCI machine learning repository:

\url{https://archive.ics.uci.edu/dataset/186/wine+quality}
}

\references{
Cortez, P., Cerdeira, A., Almeida, F., Matos, T., and Reis, J. (2009). Modeling wine preferences by data mining from physicochemical properties. \emph{Decision support systems}, 47(4), 547-553.
}

\seealso{ \code{\link{redWines}}, \code{\link{adult}}, \code{\link{risk}}, \code{\link{churn}}, \code{\link{churnTel}}, \code{\link{bank}}, \code{\link{advertising}}, \code{\link{marketing}}, \code{\link{insurance}}, \code{\link{cereal}}, \code{\link{housePrice}}, \code{\link{house}} }

\examples{
data( whiteWines )

str( whiteWines )
}

\keyword{ datasets }
