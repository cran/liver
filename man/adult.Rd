\name{adult}
\alias{adult}
\docType{data}
\title{ adult data set }

\description{ 
The adult dataset was collected from the US Census Bureau and the primary task is to predict whether a given adult makes more than $50K a year based attributes such as education, hours of work per week, etc. The target feature is \emph{income}, a factor with levels "<=50K" and ">50K", and the remaining 14 variables are predictors.
}

\usage{ data( adult ) }

\format{ 

The \code{adult} dataset, as a data frame, contains \eqn{48598} rows and \eqn{15} columns (variables/features). The \eqn{15} variables are:
 \itemize{

\item \code{age}: age in years.
\item \code{workclass}: a factor with 6 levels.
\item \code{demogweight}: the demographics to describe a person.
\item \code{education}: a factor with 16 levels.
\item \code{education.num}: number of years of education.
\item \code{marital.status}: a factor with 5 levels.
\item \code{occupation}: a factor with 15 levels.
\item \code{relationship}: a factor with 6 levels.
\item \code{race}: a factor with 5 levels.
\item \code{gender}: a factor with levels "Female","Male".
\item \code{capital.gain}: capital gains.
\item \code{capital.loss}: capital losses.
\item \code{hours.per.week}: number of hours of work per week.
\item \code{native.country}: a factor with 42 levels.
\item \code{income}: yearly income as a factor with levels "<=50K" and ">50K".
  }
}

\details{
This dataset can be downloaded from the UCI machine learning repository:

\url{http://www.cs.toronto.edu/~delve/data/adult/desc.html}

A detailed description of the dataset can be found in the UCI documentation at: 

\url{http://www.cs.toronto.edu/~delve/data/adult/adultDetail.html}
}

\references{
Kohavi, R. (1996). Scaling up the accuracy of naive-bayes classifiers: A decision-tree hybrid. \emph{Kdd}.
}

\seealso{ \code{\link{risk}}, \code{\link{churn}}, \code{\link{churnTel}}, \code{\link{bank}}, \code{\link{advertising}}, \code{\link{marketing}}, \code{\link{insurance}}, \code{\link{cereal}}, \code{\link{housePrice}}, \code{\link{house}} }

\examples{
data( adult )

str( adult )
}

\keyword{ datasets }
