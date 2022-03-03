\name{churnTel}
\alias{churnTel}

\docType{data}

\title{ churnTel dataset }

\description{ 
Customer \emph{churn} occurs when customers stop doing business with a company, also known as customer attrition. The data set contains \eqn{7043} rows (customers) and \eqn{21} columns (features). The "Churn" column is our target which indicate whether customer churned (left the company) or not.   
}

\usage{ data( churnTel ) }

\format{ 
The \code{churnTel} dataset, as a data frame, contains \eqn{7043} rows (customers) and \eqn{21} columns (variables/features). The \eqn{21} variables are:
 \itemize{
\item \code{customer.ID}: Customer ID.
\item \code{gender}: Whether the customer is a male or a female.
\item \code{senior.citizen}: Whether the customer is a senior citizen or not (1, 0).
\item \code{partner}: Whether the customer has a partner or not (yes, no).
\item \code{dependent}: Whether the customer has dependents or not (yes, no).
\item \code{tenure}: Number of months the customer has stayed with the company.
\item \code{phone.service}: Whether the customer has a phone service or not (yes, no).
\item \code{multiple.lines}: Whether the customer has multiple lines or not (yes, no, no phone service).
\item \code{internet.service}: Customer's internet service provider (DSL, fiber optic, no).
\item \code{online.security}: Whether the customer has online security or not (yes, no, no internet service).
\item \code{online.backup}: Whether the customer has online backup or not (yes, no, no internet service).
\item \code{device.protection}: Whether the customer has device protection or not (yes, no, no internet service).
\item \code{tech.support}: Whether the customer has tech support or not (yes, no, no internet service).
\item \code{streaming.TV}: Whether the customer has streaming TV or not (yes, no, no internet service).
\item \code{streaming.movie}: Whether the customer has streaming movies or not (yes, no, no internet service).
\item \code{contract}: The contract term of the customer (month to month, 1 year, 2 year).
\item \code{paperless.bill}: Whether the customer has paperless billing or not (yes, no).
\item \code{payment.method}: The customer's payment method (electronic check, mail check, bank transfer, credit card).
\item \code{monthly.charge}: The amount charged to the customer monthly.
\item \code{total.charges}: The total amount charged to the customer.
\item \code{churn}: Whether the customer churned or not (yes or no).
  }
}

\details{
For more information related to the dataset see:
\url{https://www.kaggle.com/blastchar/telco-customer-churn}
}

\seealso{ \code{\link{adult}}, \code{\link{risk}}, \code{\link{churn}}, \code{\link{bank}}, \code{\link{advertising}}, \code{\link{marketing}}, \code{\link{insurance}}, \code{\link{cereal}}, \code{\link{housePrice}}, \code{\link{house}} }

\examples{
data( churnTel )

str( churnTel )
}

\keyword{ datasets }
