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
\item \code{education.num}: an ordinal encoding of the 'education' feature.
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

\source{
The data are based on the Adult, or Census Income, dataset from the
UCI Machine Learning Repository. The original extraction was performed
by Barry Becker from the 1994 Census database.

The dataset is also associated with DOI:

\doi{10.24432/C5XW20}
}

\references{
Kohavi, R. and Becker, B. (1996). Adult. UCI Machine Learning Repository. \doi{10.24432/C5XW20}

Kohavi, R. (1996). Scaling up the accuracy of naive-bayes classifiers: A decision-tree hybrid. \emph{Kdd}.

Reza Mohammadi (2025). Data Science Foundations and Machine Learning with R: From Data to Decisions. \url{https://book-data-science-r.netlify.app}.
}

\seealso{ \code{\link[liver]{bank}}, 
          \code{\link[liver]{churn_mlc}}, 
          \code{\link[liver]{churn}}, 
          \code{\link[liver]{churn_tel}}, 
          \code{\link[liver]{risk}}, 
          \code{\link[liver]{cereal}}, 
          \code{\link[liver]{advertising}}, 
          \code{\link[liver]{marketing}}, 
          \code{\link[liver]{drug}}, 
          \code{\link[liver]{house}}, 
          \code{\link[liver]{house_price}}, 
          \code{\link[liver]{red_wines}}, 
          \code{\link[liver]{white_wines}}, 
          \code{\link[liver]{insurance}}, 
          \code{\link[liver]{caravan}}, 
          \code{\link[liver]{loan}}
          }

\examples{
data(adult)
str(adult)
}

\keyword{ datasets }
