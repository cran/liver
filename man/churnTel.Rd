\name{churnTel}
\alias{churnTel}

\docType{data}

\title{ churnTel dataset }

\description{ 
Customer \emph{churn} occurs when customers stop doing business with a company, also known as customer attrition. The data set contains \eqn{7043} rows (customers) and \eqn{21} columns (features). The "Churn" column is our target which indicate whether customer churned (left the company) or not.   
}

\usage{ data( churnTel ) }

\format{ 
The \code{churnTel} dataset, as a data frame, contains \eqn{7043} rows (customers) and \eqn{21} columns (variables/features). 
}

\details{
For more information related to the dataset see:

\url{https://www.kaggle.com/blastchar/telco-customer-churn}
}

\seealso{ \code{\link{adult}}, \code{\link{classifyRisk}}, \code{\link{churn}}, \code{\link{bank}}, \code{\link{advertising}}, \code{\link{marketing}}, \code{\link{insurance}}, \code{\link{cereal}}, \code{\link{housePrice}}, \code{\link{house}} }

\examples{
data( churnTel )

str( churnTel )
}

\keyword{ datasets }
