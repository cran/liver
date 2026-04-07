\name{mortgage}
\alias{mortgage}
\docType{data}
\title{Mortgage data set}

\description{
The \emph{mortgage} dataset contains 850 records and 8 variables. 
The target variable is \emph{risk}, a factor with two levels, \code{"low"} and \code{"high"}. 
The remaining seven variables serve as predictors. 
The dataset was simulated to represent a realistic mortgage application setting.
}

\usage{
data(mortgage)
}

\format{
A data frame with \eqn{850} rows (applicants) and \eqn{8} variables:
\itemize{
  \item \code{age}: Age in years.
  \item \code{income}: Annual income.
  \item \code{savings}: Total savings.
  \item \code{employment_status}: A factor with levels \code{"permanent"}, \code{"temporary"}, \code{"self_employed"}, and \code{"unemployed"}.
  \item \code{credit_history}: A factor with levels \code{"poor"}, \code{"average"}, and \code{"good"}.
  \item \code{debt_level}: A factor with levels \code{"low"}, \code{"medium"}, and \code{"high"}.
  \item \code{loan_amount}: Requested loan amount.
  \item \code{risk}: A factor with levels \code{"low"} and \code{"high"}.
}
}

\details{
The dataset was generated using a hybrid latent simulation approach. Continuous variables were simulated with dependence, and categorical variables were derived from latent scores to create realistic relationships among applicant characteristics, financial indicators, and mortgage risk.
}

\source{
Simulated data generated for illustration and teaching purposes.
}

\references{
Reza Mohammadi (2025). Data Science Foundations and Machine Learning with R: From Data to Decisions. \url{https://book-data-science-r.netlify.app}.
}

\seealso{ \code{\link[liver]{bank}}, 
          \code{\link[liver]{churn_mlc}}, 
          \code{\link[liver]{churn}}, 
          \code{\link[liver]{churn_tel}}, 
          \code{\link[liver]{adult}}, 
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
          \code{\link[liver]{fertilizer}}, 
          \code{\link[liver]{corona}}
          }

\examples{
data(mortgage)
str(mortgage)
}

\keyword{ datasets }
