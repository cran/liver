\name{doctor_visits}
\alias{doctor_visits}
\docType{data}
\title{Doctor Visits and Health Care Utilization Data}
\description{
A dataset containing information on individuals' doctor visit counts, demographic
characteristics, income, illness burden, reduced activity days, self-reported
health status, and indicators of health care coverage and chronic conditions.
}
\usage{
data(doctor_visits)
}
\format{
A data frame with 5190 observations and 12 variables:
\describe{
  \item{age}{Age of the individual.}
  \item{income}{Income level of the individual.}
  \item{illness}{Number of illnesses experienced by the individual.}
  \item{reduced}{Number of days with reduced activity.}
  \item{health}{Self-reported health score.}
  \item{gender}{Gender of the individual: \code{"male"} or \code{"female"}.}
  \item{private}{Whether the individual has private health insurance: \code{"yes"} or \code{"no"}.}
  \item{freepoor}{Whether the individual is covered by free government health care due to low income: \code{"yes"} or \code{"no"}.}
  \item{freerepat}{Whether the individual is covered by free government health care due to repatriation status: \code{"yes"} or \code{"no"}.}
  \item{nchronic}{Whether the individual has a chronic condition that is not limiting: \code{"yes"} or \code{"no"}.}
  \item{lchronic}{Whether the individual has a chronic condition that is limiting: \code{"yes"} or \code{"no"}.}
  \item{visits}{Number of doctor visits (target variable).}
}
}
\details{
This dataset was adapted for inclusion in the \pkg{liver} package and can be
used to illustrate methods for count data modeling, exploratory data analysis,
and regression techniques such as Poisson regression in R.
}
\source{
Originally distributed with the \pkg{AER} package.
}

\references{
Mullahy, J. (1997). Heterogeneity, Excess Zeros, and the Structure of Count Data Models.  
\emph{Journal of Applied Econometrics}, 12:337--350.

Cameron, A.C. and Trivedi, P.K. (1986). Econometric Models Based on Count Data: 
Comparisons and Applications of Some Estimators and Tests.
\emph{Journal of Applied Econometrics}, 1:29--53.

Cameron, A.C. and Trivedi, P.K. (1998). \emph{Regression Analysis of Count Data}. 
Cambridge: Cambridge University Press.

Reza Mohammadi (2025). Data Science Foundations and Machine Learning with R: From Data to Decisions. \url{https://book-data-science-r.netlify.app}.
}

\seealso{ 
          \code{\link[liver]{bike_demand}}, 
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
data(doctor_visits)

str(doctor_visits)
summary(doctor_visits)
}
\keyword{datasets}













