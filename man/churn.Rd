\name{churn}
\alias{churn}
\docType{data}
\title{ Churn data set }

\description{ 
This dataset comes from IBM Sample Data Sets. Customer \emph{churn} occurs when customers stop doing business with a company, also known as customer attrition. The data set contains \eqn{5000} rows (customers) and \eqn{20} columns (features). The "Churn" column is our target which indicate whether customer churned (left the company) or not.   
}

\usage{ data( churn ) }

\format{ 

The \code{churn} dataset, as a data frame, contains \eqn{5000} rows (customers) and \eqn{20} columns (variables/features). The \eqn{20} variables are:
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

\references{
Larose, D. T. and Larose, C. D. (2014). Discovering knowledge in data: an introduction to data mining. \emph{John Wiley & Sons}.
}


\seealso{ \code{\link{adult}}, \code{\link{classifyRisk}}, \code{\link{churnTel}}, \code{\link{bank}}, \code{\link{advertising}}, \code{\link{marketing}}, \code{\link{insurance}}, \code{\link{cereal}}, \code{\link{housePrice}}, \code{\link{house}} }

\examples{
data( churn )

str( churn )
}

\keyword{ datasets }
