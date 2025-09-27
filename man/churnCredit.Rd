\name{churnCredit}
\alias{churnCredit}

\docType{data}

\title{ Churn dataset for Credit Card Customers }

\description{ 
Customer \emph{churn} occurs when customers stop doing business with a company, also known as customer attrition. the data set contains \eqn{10127} rows (customers) and \eqn{21} columns (features). the "churn" column is our target which indicate whether customer churned (left the company) or not.   
}

\usage{ data(churnCredit) }

\format{ 
the \code{churnCredit} dataset, as a data frame, contains \eqn{10127} rows (customers) and \eqn{21} columns (variables/features). the \eqn{21} variables are:
 \itemize{
\item \code{customer.ID}: Customer ID.
\item \code{gender}: Whether the customer is a male or a female.
\item \code{age}: Customer's Age in Years.
\item \code{educaton}: Educational Qualification of the account holder (example: high school, college graduate, etc.)
\item \code{marital.status}: Married, Single, Divorced, Unknown
\item \code{income}: Annual Income (in Dollar). Category of the account holder (< $40K, $40K - 60K, $60K - $80K, $80K-$120K, > $120K).
\item \code{dependent.counts}: Number of dependent counts.
\item \code{card.category}: Type of Card (Blue, Silver, Gold, Platinum).
\item \code{months.on.book}: Period of relationship with bank.
\item \code{relationship.count}: Total number of products held by the customer.
\item \code{months.inactive}: Number of months inactive in the last 12 months.
\item \code{contacts.count.12}: Number of Contacts in the last 12 months.
\item \code{credit.limit}: Credit Limit on the Credit Card.
\item \code{revolving.balance}: Total Revolving Balance on the Credit Card.
\item \code{open.to.buy}: Open to Buy Credit Line (Average of last 12 months).
\item \code{transaction.amount.Q4.Q1}: Change in Transaction Amount (Q4 over Q1).
\item \code{transaction.amount.12}: Total Transaction Amount (Last 12 months).
\item \code{transaction.count}: Total Transaction Count (Last 12 months).
\item \code{transaction.change}: Change in Transaction Count (Q4 over Q1).
\item \code{utilization.ratio}: Average Card Utilization Ratio.
\item \code{churn}: Whether the customer churned or not (yes or no).
  }
}

\details{
For more information related to the dataset see:  \cr
\url{https://www.kaggle.com/sakshigoyal7/credit-card-customers}
}

\seealso{ \code{\link[liver]{adult}}, \code{\link[liver]{risk}}, \code{\link[liver]{churn}}, \code{\link[liver]{churnTel}}, \code{\link[liver]{bank}}, \code{\link[liver]{advertising}}, \code{\link[liver]{marketing}}, \code{\link[liver]{insurance}}, \code{\link[liver]{cereal}}, \code{\link[liver]{housePrice}}, \code{\link[liver]{house}} }

\examples{
data(churnCredit)
str(churnCredit)
}

\keyword{ datasets }
