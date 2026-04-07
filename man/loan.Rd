\name{loan}
\alias{loan}
\docType{data}
\title{Loan Application and Approval Data}
\description{
A dataset containing information on loan applicants and their financial profiles,
including demographic characteristics, employment status, income, loan details,
credit score, asset values, and loan approval outcome.
}
\usage{
data(loan)
}
\format{
A data frame with 4269 observations and 13 variables:
\describe{
  \item{loan_id}{Unique identifier for each loan application; not intended as a predictor in modeling.}
  \item{no_of_dependents}{Number of dependents of the applicant.}
  \item{education}{Education level of the applicant: \code{"graduate"} or \code{"not-graduate"}.}
  \item{self_employed}{Whether the applicant is self-employed: \code{"yes"} or \code{"no"}.}
  \item{income_annum}{Annual income of the applicant.}
  \item{loan_amount}{Requested loan amount.}
  \item{loan_term}{Loan term.}
  \item{cibil_score}{Applicant's CIBIL credit score.}
  \item{residential_assets_value}{Value of the applicant's residential assets.}
  \item{commercial_assets_value}{Value of the applicant's commercial assets.}
  \item{luxury_assets_value}{Value of the applicant's luxury assets.}
  \item{bank_asset_value}{Value of the applicant's bank assets.}
  \item{loan_status}{Loan application outcome: \code{"approved"} or \code{"rejected"}.}
}
}
\details{
This dataset was obtained from Kaggle and renamed \code{loan} for inclusion
in the \pkg{liver} package. It can be used to illustrate methods for
classification, exploratory data analysis, and predictive modeling in R.
}
\source{
\url{https://www.kaggle.com/datasets/architsharma01/loan-approval-prediction-dataset}
}

\references{
Reza Mohammadi (2025). Data Science Foundations and Machine Learning with R: From Data to Decisions. \url{https://book-data-science-r.netlify.app}.
}

\seealso{ \code{\link[liver]{mortgage}}, 
          \code{\link[liver]{bank}}, 
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
data(loan)
str(loan)
summary(loan)

table(loan$loan_status)
}
\keyword{datasets}













