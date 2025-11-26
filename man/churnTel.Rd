\name{churnTel}
\alias{churnTel}

\docType{data}

\title{ churnTel dataset }

\description{ 
The \emph{churnTel} data set contains \eqn{7043} rows (customers) and \eqn{21} columns (features). The \emph{churn} column is our target which indicate whether customer churned (left the company) or not.   
}

\usage{ data(churnTel) }

\format{ 
the \code{churnTel} dataset, as a data frame, contains \eqn{7043} rows (customers) and \eqn{21} columns (variables/features). the \eqn{21} variables are:
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
\item \code{contract}: the contract term of the customer (month to month, 1 year, 2 year).
\item \code{paperless.bill}: Whether the customer has paperless billing or not (yes, no).
\item \code{payment.method}: the customer's payment method (electronic check, mail check, bank transfer, credit card).
\item \code{monthly.charge}: the amount charged to the customer monthly.
\item \code{total.charges}: the total amount charged to the customer.
\item \code{churn}: Whether the customer churned or not (yes or no).
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

\seealso{ \code{\link[liver]{bank}}, 
          \code{\link[liver]{churn}}, 
          \code{\link[liver]{churnCredit}}, 
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
data(churnTel)
str(churnTel)
}

\keyword{ datasets }
