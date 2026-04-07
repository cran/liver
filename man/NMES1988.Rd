\name{NMES1988}
\alias{NMES1988}
\docType{data}
\title{Health Care Utilization Data from the 1988 NMES Survey}
\description{
The \code{NMES1988} dataset contains information on medical service use among
older adults in the United States. In addition to several counts of health care
utilization, it includes demographic, socioeconomic, insurance, and health-related
variables that are useful for studying patterns in demand for care.
}
\usage{
data(NMES1988)
}
\format{
A data frame with 4406 observations on 19 variables:
\describe{
  \item{visits}{Number of visits to a physician's office.}
  \item{nvisits}{Number of visits to a non-physician provider's office.}
  \item{ovisits}{Number of outpatient visits involving a physician.}
  \item{novisits}{Number of outpatient visits not involving a physician.}
  \item{emergency}{Number of emergency room visits.}
  \item{hospital}{Number of hospital admissions.}
  \item{health}{Self-rated health status, recorded as \code{"poor"}, \code{"average"}, or \code{"excellent"}.}
  \item{chronic}{Number of chronic medical conditions.}
  \item{adl}{Indicator of limitation in activities of daily living, with levels \code{"limited"} and \code{"normal"}.}
  \item{region}{Region of residence, with categories \code{"northeast"}, \code{"midwest"}, \code{"west"}, and \code{"other"}.}
  \item{age}{Age measured in decades.}
  \item{afam}{Indicator for African-American ethnicity: \code{"yes"} or \code{"no"}.}
  \item{gender}{Gender of the individual.}
  \item{married}{Marital status indicator: \code{"yes"} or \code{"no"}.}
  \item{school}{Years of schooling completed.}
  \item{income}{Family income measured in units of 10,000 US dollars.}
  \item{employed}{Employment status indicator: \code{"yes"} or \code{"no"}.}
  \item{insurance}{Indicator for private insurance coverage: \code{"yes"} or \code{"no"}.}
  \item{medicaid}{Indicator for Medicaid coverage: \code{"yes"} or \code{"no"}.}
}
}
\details{
This dataset is included in the \pkg{liver} package for teaching and applied
work in data science and statistical modeling. It is especially suitable for
examples involving count outcomes, exploratory analysis, Poisson regression,
and related generalized linear models.

Because the dataset contains several different measures of medical utilization,
it can also be used to compare alternative response variables and to discuss
how health status, insurance coverage, and socioeconomic factors relate to
health care use.
}

\source{
Derived from the National Medical Expenditure Survey (NMES) conducted in 1987
and 1988. The version included here is adapted from material distributed through
the \pkg{AER} package.
}

\references{
Deb, P. and Trivedi, P. K. (1997).
Demand for Medical Care by the Elderly: A Finite Mixture Approach.
\emph{Journal of Applied Econometrics}, 12(3), 313--336.

Cameron, A. C. and Trivedi, P. K. (1998).
\emph{Regression Analysis of Count Data}.
Cambridge: Cambridge University Press.

Zeileis, A., Kleiber, C., and Jackman, S. (2008).
Regression Models for Count Data in R.
\emph{Journal of Statistical Software}, 27(8), 1--25.

Mohammadi, R. (2025).
\emph{Data Science Foundations and Machine Learning with R: From Data to Decisions}.
\url{https://book-data-science-r.netlify.app}
}

\seealso{ 
          \code{\link[liver]{doctor_visits}}, 
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
          \code{\link[liver]{fertilizer}}, 
          \code{\link[liver]{corona}}
          }

\examples{
data(NMES1988)

str(NMES1988)
}
\keyword{datasets}













