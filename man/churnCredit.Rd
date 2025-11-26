\name{churnCredit}
\alias{churnCredit}

\docType{data}

\title{ Churn dataset for Credit Card Customers }

\description{ 
The \emph{churnCredit} data set contains \eqn{10127} rows (customers) and \eqn{21} columns (features). The \emph{churn} column is our target which indicate whether customer churned (left the company) or not.   
}

\usage{ data(churnCredit) }

\format{ 
the \code{churnCredit} dataset, as a data frame, contains \eqn{10127} rows (customers) and \eqn{21} columns (variables/features). the \eqn{21} variables are:
 \itemize{
\item \code{customer.ID}: Unique identifier for each account holder.
\item \code{age}: Age of the customer, in years.
\item \code{gender}: Gender of the account holder.
\item \code{education}: Educational qualification (high-school, college, graduate, uneducated, post-graduate, doctorate, unknown).
\item \code{marital}: Marital status (married, single, divorced, unknown).
\item \code{income}: Annual income bracket (less than $40K, $40K-$60K, $60K-$80K, $80K-$120K, over $120K, unknown).
\item \code{card.category}: Credit card type (blue, silver, gold, platinum).
\item \code{dependent.count}: Number of dependents.
\item \code{months.on.book}: Tenure with the bank, in months.
\item \code{relationship.count}: Total number of products held by the customer (1-6).
\item \code{months.inactive}: Number of inactive months in the past 12 months.
\item \code{contacts.count.12}: Number of customer service contacts in the past 12 months.
\item \code{credit.limit}: Total credit card limit.
\item \code{revolving.balance}: Current revolving balance on the credit card.
\item \code{available.credit}: Available credit line, representing the unused portion of the credit limit. Calculated as \code{credit.limit - revolving.balance}.
\item \code{transaction.amount.12}: Total transaction amount in the past 12 months.
\item \code{transaction.count.12}: Total number of transactions in the past 12 months.
\item \code{ratio.amount.Q4.Q1}: Ratio of total transaction amount in the fourth quarter to that in the first quarter.
\item \code{ratio.count.Q4.Q1}: Ratio of total transaction count in the fourth quarter to that in the first quarter.
\item \code{utilization.ratio}: Average credit utilization ratio, defined as \code{revolving.balance / credit.limit}.
\item \code{churn}: Indicator of whether the account was closed (\code{yes}) or remained active (\code{no}).
  }
}

\details{
For more information related to the dataset see:  \cr
\url{https://www.kaggle.com/sakshigoyal7/credit-card-customers}
}

\source{
This dataset is originally from https://leaps.analyttica.com/home
}

\references{
Reza Mohammadi (2025). Data Science Foundations and Machine Learning with R: From Data to Decisions. \url{https://book-data-science-r.netlify.app}.
}

\seealso{ \code{\link[liver]{bank}}, 
          \code{\link[liver]{churn}}, 
          \code{\link[liver]{churnTel}}, 
          \code{\link[liver]{adult}}, 
          \code{\link[liver]{risk}}, 
          \code{\link[liver]{cereal}}, 
          \code{\link[liver]{advertising}}, 
          \code{\link[liver]{marketing}}, 
          \code{\link[liver]{drug}}, 
          \code{\link[liver]{house}}, 
          \code{\link[liver]{housePrice}}, 
          \code{\link[liver]{redWines}}, 
          \code{\link[liver]{whiteWines}}, 
          \code{\link[liver]{insurance}}, 
          \code{\link[liver]{caravan}}, 
          \code{\link[liver]{fertilizer}}, 
          \code{\link[liver]{corona}}
          }

\examples{
data(churnCredit)

str(churnCredit)
}

\keyword{ datasets }
