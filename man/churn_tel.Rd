\name{churn_tel}
\alias{churn_tel}

\docType{data}

\title{ churn_tel dataset }

\description{ 
The \emph{churn_tel} data set contains \eqn{7043} rows (customers) and \eqn{21} columns (features). The \emph{churn} column is our target which indicate whether customer churned (left the company) or not.   
}

\usage{ data(churn_tel) }

\format{
A data frame with 7,032 rows and 21 variables. Each row represents one telecommunications customer.
\describe{
  \item{\code{customer_ID}}{Unique customer identifier. A factor with 7,043 levels, of which 7,032 occur in the data.}
  \item{\code{gender}}{Customer gender. A factor with levels \code{"female"} and \code{"male"}.}
  \item{\code{senior_citizen}}{Integer indicator equal to \code{1} if the customer is a senior citizen and \code{0} otherwise.}
  \item{\code{partner}}{Whether the customer has a partner. A factor with levels \code{"yes"} and \code{"no"}.}
  \item{\code{dependent}}{Whether the customer has dependents. A factor with levels \code{"yes"} and \code{"no"}.}
  \item{\code{tenure}}{Number of months the customer has been with the company.}
  \item{\code{phone_service}}{Whether the customer has phone service. A factor with levels \code{"yes"} and \code{"no"}.}
  \item{\code{multiple_lines}}{Whether the customer has multiple phone lines. A factor with levels \code{"yes"}, \code{"no-phone-service"}, and \code{"no"}.}
  \item{\code{internet_service}}{Type of internet service. A factor with levels \code{"DSL"}, \code{"fiber-optic"}, and \code{"no"}.}
  \item{\code{online_security}}{Whether the customer has online security. A factor with levels \code{"yes"}, \code{"no-internet-service"}, and \code{"no"}.}
  \item{\code{online_backup}}{Whether the customer has online backup. A factor with levels \code{"yes"}, \code{"no-internet-service"}, and \code{"no"}.}
  \item{\code{device_protection}}{Whether the customer has device protection. A factor with levels \code{"yes"}, \code{"no-internet-service"}, and \code{"no"}.}
  \item{\code{tech_support}}{Whether the customer has technical support. A factor with levels \code{"yes"}, \code{"no-internet-service"}, and \code{"no"}.}
  \item{\code{streaming_TV}}{Whether the customer has TV streaming service. A factor with levels \code{"yes"}, \code{"no-internet-service"}, and \code{"no"}.}
  \item{\code{streaming_movie}}{Whether the customer has movie streaming service. A factor with levels \code{"yes"}, \code{"no-internet-service"}, and \code{"no"}.}
  \item{\code{contract}}{Customer's contract term. A factor with levels \code{"month-to-month"}, \code{"1-year"}, and \code{"2-year"}.}
  \item{\code{paperless_bill}}{Whether the customer uses paperless billing. A factor with levels \code{"yes"} and \code{"no"}.}
  \item{\code{payment_method}}{Customer's payment method. A factor with levels \code{"bank-transfer"}, \code{"credit-card"}, \code{"electronic-check"}, and \code{"mail-check"}.}
  \item{\code{monthly_charge}}{Amount charged to the customer per month.}
  \item{\code{total_charge}}{Total amount charged to the customer over their tenure with the company.}
  \item{\code{churn}}{Customer churn indicator. A factor with levels \code{"yes"} (the customer left the company) and \code{"no"} (the customer stayed with the company).}
}
}

\details{
For more information related to the dataset see:  \cr
\url{https://www.kaggle.com/blastchar/telco-customer-churn}
}

\source{
This dataset comes from the IBM Sample Data Sets: \cr
\url{https://community.ibm.com/community/user/blogs/steven-macko/2019/07/11/telco-customer-churn-1113}
}

\references{
Reza Mohammadi (2025). Data Science Foundations and Machine Learning with R: From Data to Decisions. \url{https://book-data-science-r.netlify.app}.
}

\seealso{ \code{\link[liver]{churn_mlc}}, \code{\link[liver]{churn}}}

\examples{
data(churn_tel)
str(churn_tel)
}

\keyword{ datasets }
