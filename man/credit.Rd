\name{credit}
\alias{credit}
\docType{data}
\title{South German Credit Data}
\description{
A dataset containing information on credit applicants, including account status,
credit history, loan purpose, credit amount, savings, employment duration,
personal characteristics, property, housing, and other financial attributes.
The outcome variable indicates whether the applicant represents a good or bad credit risk.
}
\usage{
data(credit)
}
\format{
A data frame with 1000 observations and 21 variables:
\describe{
  \item{status}{Status of the debtor's checking account with the bank.}
  \item{duration}{Credit duration in months.}
  \item{credit_history}{History of compliance with previous or concurrent credit contracts.}
  \item{purpose}{Purpose for which the credit is needed.}
  \item{amount}{Credit amount in Deutsche Mark (DM).}
  \item{savings}{Debtor's savings.}
  \item{employment_duration}{Duration of the debtor's employment with the current employer.}
  \item{installment_rate}{Credit installments as a percentage of the debtor's disposable income.}
  \item{personal_status_sex}{Combined information on personal status and sex.}
  \item{other_debtors}{Whether there is another debtor or a guarantor for the credit.}
  \item{present_residence}{Length of time the debtor has lived in the present residence.}
  \item{property}{The debtor's most valuable property.}
  \item{age}{Age in years.}
  \item{other_installment_plans}{Installment plans from providers other than the credit-giving bank.}
  \item{housing}{Type of housing the debtor lives in.}
  \item{number_credits}{Number of credits the debtor has or had at this bank, including the current one.}
  \item{job}{Quality of the debtor's job.}
  \item{people_liable}{Number of persons financially dependent on the debtor.}
  \item{telephone}{Whether a telephone landline is registered in the debtor's name.}
  \item{foreign_worker}{Whether the debtor is a foreign worker.}
  \item{credit_risk}{Credit risk outcome: \code{"good risk"} or \code{"bad risk"}.}
}
}

\details{
The South German Credit data are a corrected and documented version of the widely
used German credit data. The dataset contains 700 good and 300 bad credits and
covers actual credit data from 1973 to 1975, with bad credits heavily oversampled.
It can be used to illustrate methods for classification, exploratory data analysis,
and predictive modeling in R.
}

\source{
UCI Machine Learning Repository:
\url{https://archive.ics.uci.edu/dataset/573/south+german+credit+update}

South German Credit [Dataset]. (2020). UCI Machine Learning Repository.
\doi{10.24432/C5QG88}
}
\references{
Gr\"omping, U. (2019). South German credit data: Correcting a widely used data set.
}

\seealso{ 
          \code{\link[liver]{mortgage}}, 
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
          \code{\link[liver]{loan}}
          }

\examples{
data(credit)

str(credit)
summary(credit)
}
\keyword{datasets}



















