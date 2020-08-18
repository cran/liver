\name{adult}
\alias{adult}
\docType{data}
\title{ adult data set }

\description{ 
The adult dataset contains \eqn{15} features and \eqn{48842} records. It was collected from the US Census bureau and the primary task is to predict whether a given adult makes more than $50K a year based attributes such as education, hours of work per week, etc. The target feature is \emph{income}, a factor with levels "<=50K" and ">50K", and the remaining 14 variables are predictors.
}

\usage{ data( adult ) }

\format{ 

The \code{adult} dataset, as a data frame, contains \eqn{48842} rows (customers) and \eqn{15} columns (variables/features). The \eqn{15} variables are:
 \itemize{

\item \code{age}: age in years.
\item \code{workclass}: a factor with 9 levels.
\item \code{fnlwgt}: the demographics to describe a person.
\item \code{education}: a factor with 16 levels.
\item \code{education.num}: number of years of education.
\item \code{marital.status}: a factor with 7 levels.
\item \code{occupation}: a factor with 15 levels.
\item \code{relationship}: a factor with 6 levels.
\item \code{race}: a factor with 5 levels "Female","Male".
\item \code{gender}: a factor with 2 levels.
\item \code{capital.gain}: capital gains.
\item \code{capital.loss}: capital losses.
\item \code{hours.per.week}: number of hours of work per week.
\item \code{native.country}: a factor with 42 levels.
\item \code{income}: yearly income as a factor with levels "<=50K" and ">50K".
  }
}

\details{
This dataset can be downloaded from the UCI machine learning repository:

http://www.cs.toronto.edu/~delve/data/adult/desc.html

The detailed description on the dataset can be found in the UCI documentation

http://www.cs.toronto.edu/~delve/data/adult/adultDetail.html
}

\references{
Kohavi, R. (1996). Scaling up the accuracy of naive-bayes classifiers: A decision-tree hybrid. \emph{Kdd}.
}

\seealso{ \code{\link{churn}}, \code{\link{churnTel}}, \code{\link{classifyRisk}}, \code{\link{housePrice}} }

\examples{
data( adult )

str( adult )
}

\keyword{ datasets }
