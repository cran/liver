\name{risk}
\alias{risk}
\docType{data}
\title{ Risk data set }

\description{
The \emph{risk} dataset contains 246 records and 6 variables. The target variable is \emph{risk}, a factor with two levels ("good risk" and "bad risk"). The remaining five variables serve as predictors. The dataset was simulated to reflect a realistic real-world scenario.
}

\usage{ data(risk) }

\format{ 

the \code{risk} dataset, as a data frame, contains \eqn{246} rows (customers) and \eqn{6} columns (variables/features). the \eqn{6} variables are:
 \itemize{

\item \code{age}: age in years.
\item \code{marital}: A factor with levels "single", "married", and "other".
\item \code{income}: yearly income.
\item \code{mortgage}: A factor with levels "yes" and "no".
\item \code{nr_loans}: Number of loans that constomers have.
\item \code{risk}: A factor with levels "good risk" and "bad risk".
  }
}

\references{
Larose, D. T. and Larose, C. D. (2014). Discovering knowledge in data: an introduction to data mining. \emph{John Wiley & Sons}.

Reza Mohammadi (2025). Data Science Foundations and Machine Learning with R: From Data to Decisions. \url{https://book-data-science-r.netlify.app}.
}

\seealso{ \code{\link[liver]{bank}}, 
          \code{\link[liver]{churn}}, 
          \code{\link[liver]{churnCredit}}, 
          \code{\link[liver]{churnTel}}, 
          \code{\link[liver]{adult}}, 
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
data(risk)
str(risk)
}

\keyword{ datasets }
