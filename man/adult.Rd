\name{adult}
\alias{adult}
\docType{data}
\title{ adult data set }

\description{ 
the adult dataset was collected from the US Census Bureau and the primary task is to predict whether a given adult makes more than $50K a year based attributes such as education, hours of work per week, etc. the target feature is \emph{income}, a factor with levels "<=50K" and ">50K", and the remaining 14 variables are predictors.
}

\usage{ data(adult) }

\format{ 

the \code{adult} dataset, as a data frame, contains \eqn{48598} rows and \eqn{15} columns (variables/features). the \eqn{15} variables are:
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
For more information related to the dataset see the UCI Machine Learning Repository:  \cr
\url{http://www.cs.toronto.edu/~delve/data/adult/desc.html}  \cr
\url{http://www.cs.toronto.edu/~delve/data/adult/adultDetail.html}
}

\source{
This dataset comes from the UCI repository of machine learning databases: \cr
\url{https://archive.ics.uci.edu}
}

\references{
Kohavi, R. (1996). Scaling up the accuracy of naive-bayes classifiers: A decision-tree hybrid. \emph{Kdd}.

Reza Mohammadi (2025). Data Science Foundations and Machine Learning with R: From Data to Decisions. \url{https://book-data-science-r.netlify.app}.
}

\seealso{ \code{\link[liver]{bank}}, 
          \code{\link[liver]{churn}}, 
          \code{\link[liver]{churnCredit}}, 
          \code{\link[liver]{churnTel}}, 
          \code{\link[liver]{risk}}, 
          \code{\link[liver]{cereal}}, 
          \code{\link[liver]{advertising}}, 
          \code{\link[liver]{marketing}}, 
          \code{\link[liver]{drug}}, 
          \code{\link[liver]{house}}, 
          \code{\link[liver]{housePrice}}, 
          \code{\link[liver]{redWines}}, 
          \code{\link[liver]{whiteWines}}, 
          \code{\link[liver]{insurance}}, 
          \code{\link[liver]{caravan}}, 
          \code{\link[liver]{fertilizer}}, 
          \code{\link[liver]{corona}}
          }

\examples{
data(adult)
str(adult)
}

\keyword{ datasets }
