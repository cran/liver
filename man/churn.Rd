\name{churn}
\alias{churn}
\docType{data}
\title{Churn data set}

\description{ 
This dataset comes from IBM Sample Data Sets. Customer \emph{churn} occurs when customers stop doing business with a company, also known as customer attrition. the data set contains \eqn{5000} rows (customers) and \eqn{20} columns (features). the "Churn" column is our target which indicate whether customer churned (left the company) or not.   
}

\usage{data(churn)}

\format{ 

A data frame with \eqn{5000} rows (customers) and \eqn{20} columns (variables/features). the \eqn{20} variables are:
 \itemize{

\item \code{state}: Categorical, for the \eqn{51} states and the District of Columbia.
\item \code{area.code}: Categorical.
\item \code{account.length}: count, how long account has been active.
\item \code{voice.plan}: Categorical, yes or no, voice mail plan.
\item \code{voice.messages}: Count, number of voice mail messages.
\item \code{intl.plan}: Categorical, yes or no, international plan.
\item \code{intl.mins}: Continuous, minutes customer used service to make international calls.
\item \code{intl.calls}: Count, total number of international calls.
\item \code{intl.charge}: Continuous, total international charge.
\item \code{day.mins}: Continuous, minutes customer used service during the day.
\item \code{day.calls}: Count, total number of calls during the day.
\item \code{day.charge}: Continuous, total charge during the day.
\item \code{eve.mins}: Continuous, minutes customer used service during the evening.
\item \code{eve.calls}: Count, total number of calls during the evening.
\item \code{eve.charge}: Continuous, total charge during the evening.
\item \code{night.mins}: Continuous, minutes customer used service during the night.
\item \code{night.calls}: Count, total number of calls during the night.
\item \code{night.charge}: Continuous, total charge during the night.
\item \code{customer.calls}: Count, number of calls to customer service.
\item \code{churn}: Categorical, yes or no. Indicator of whether the customer has left the company (yes or no).    
  }
}

\details{
For more information related to the dataset see:

OpenML: \verb{https://www.openml.org/search?type=data&sort=runs&id=40701&status=active}

 data.world: \verb{https://data.world/earino/churn}
}

\references{
Umayaparvathi, V., and Iyakutti, K. (2016). A survey on customer churn prediction in telecom industry: Datasets, methods and metrics. \emph{International Research Journal of Engineering and Technology (IRJET)}, 3(04), 1065-1070

Saha, S., Saha, C., Haque, M. M., Alam, M. G. R., and Talukder, A. (2024). ChurnNet: Deep learning enhanced customer churn prediction in telecommunication industry. \emph{IEEE access}, 12, 4471-4484.
}

\seealso{ \code{\link[liver]{adult}}, \code{\link[liver]{risk}}, \code{\link[liver]{churnTel}}, \code{\link[liver]{bank}}, \code{\link[liver]{advertising}}, \code{\link[liver]{marketing}}, \code{\link[liver]{insurance}}, \code{\link[liver]{cereal}}, \code{\link[liver]{housePrice}}, \code{\link[liver]{house}} }

\examples{
data(churn)

str(churn)
}

\keyword{datasets}
