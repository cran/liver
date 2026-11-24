\name{churn_mlc}
\alias{churn_mlc}
\docType{data}
\title{Churn data set from MLC++ machine learing}

\description{ 
This dataset originates from the MLC++ machine learning software and is used for modeling customer churn. Customer \emph{churn}, also known as customer attrition, refers to the event in which customers stop doing business with a company. The dataset contains \eqn{5000} rows (customers) and \eqn{20} columns (features). The \emph{churn} column serves as the target variable, indicating whether a customer has churned (left the company) or not.
}

\usage{data(churn_mlc)}

\format{
A data frame with 5,000 rows and 20 variables. Each row represents one telecommunications customer.
\describe{
  \item{\code{state}}{Customer's state of residence. A factor with 51 levels representing the 50 U.S. states and the District of Columbia.}
  \item{\code{area_code}}{Telephone area code. A factor with levels \code{"area_code_408"}, \code{"area_code_415"}, and \code{"area_code_510"}.}
  \item{\code{account_length}}{Length of time the customer's account has been active.}
  \item{\code{voice_plan}}{Whether the customer has a voicemail plan. A factor with levels \code{"yes"} and \code{"no"}.}
  \item{\code{voice_messages}}{Number of voicemail messages.}
  \item{\code{intl_plan}}{Whether the customer has an international calling plan. A factor with levels \code{"yes"} and \code{"no"}.}
  \item{\code{intl_mins}}{Total minutes spent on international calls.}
  \item{\code{intl_calls}}{Total number of international calls.}
  \item{\code{intl_charge}}{Total charge for international calls.}
  \item{\code{day_mins}}{Total minutes spent on daytime calls.}
  \item{\code{day_calls}}{Total number of daytime calls.}
  \item{\code{day_charge}}{Total charge for daytime calls.}
  \item{\code{eve_mins}}{Total minutes spent on evening calls.}
  \item{\code{eve_calls}}{Total number of evening calls.}
  \item{\code{eve_charge}}{Total charge for evening calls.}
  \item{\code{night_mins}}{Total minutes spent on nighttime calls.}
  \item{\code{night_calls}}{Total number of nighttime calls.}
  \item{\code{night_charge}}{Total charge for nighttime calls.}
  \item{\code{customer_calls}}{Number of calls the customer made to customer service.}
  \item{\code{churn}}{Customer churn indicator. A factor with levels \code{"yes"} (the customer left the company) and \code{"no"} (the customer stayed with the company).}
}
}

\details{
For more information related to the dataset see  \cr
- OpenML: \url{https://www.openml.org/search?type=data&sort=runs&id=40701&status=active}  \cr
- data.world: \url{https://data.world/earino/churn}
}

\source{
This dataset is originally from http://www.sgi.com/tech/mlc
}

\references{
Saha, S., Saha, C., Haque, M. M., Alam, M. G. R., and Talukder, A. (2024). ChurnNet: Deep learning enhanced customer churn prediction in telecommunication industry. \emph{IEEE access}, 12, 4471-4484.  \cr
Umayaparvathi, V., and Iyakutti, K. (2016). A survey on customer churn prediction in telecom industry: Datasets, methods and metrics. \emph{International Research Journal of Engineering and Technology (IRJET)}, 3(04), 1065-1070 \cr
Reza Mohammadi (2025). Data Science Foundations and Machine Learning with R: From Data to Decisions. \url{https://book-data-science-r.netlify.app}.
}

\seealso{ \code{\link[liver]{churn}}, \code{\link[liver]{churn_tel}}}

\examples{
data(churn_mlc)
str(churn_mlc)
}

\keyword{datasets}
