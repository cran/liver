\name{churn}
\alias{churn}
\docType{data}

\title{Credit Card Customer Churn}

\description{
The \code{churn} dataset contains demographic information, account characteristics, and transaction activity for credit card customers. The outcome variable, \code{churn}, indicates whether the customer closed the account (\code{"yes"}) or kept it active (\code{"no"}).
}

\usage{data(churn)}

\format{
A data frame with 10,127 rows and 21 variables. Each row represents one credit card customer.
\describe{
  \item{\code{customer_ID}}{Unique customer identifier.}
  \item{\code{age}}{Customer age in years.}
  \item{\code{gender}}{Gender. A factor with levels \code{"female"} and \code{"male"}.}
  \item{\code{education}}{Education level. A factor with levels \code{"uneducated"}, \code{"highschool"}, \code{"college"}, \code{"graduate"}, \code{"post-graduate"}, \code{"doctorate"}, and \code{"unknown"}.}
  \item{\code{marital}}{Marital status. A factor with levels \code{"married"}, \code{"single"}, \code{"divorced"}, and \code{"unknown"}.}
  \item{\code{income}}{Annual income bracket, in thousands of dollars. A factor with levels \code{"<40K"}, \code{"40K-60K"}, \code{"60K-80K"}, \code{"80K-120K"}, \code{">120K"}, and \code{"unknown"}.}
  \item{\code{card_category}}{Credit card type. A factor with levels \code{"blue"}, \code{"silver"}, \code{"gold"}, and \code{"platinum"}.}
  \item{\code{dependent_count}}{Number of dependents.}
  \item{\code{months_on_book}}{Number of months the customer has been with the bank.}
  \item{\code{relationship_count}}{Number of bank products the customer holds (1 to 6).}
  \item{\code{months_inactive}}{Number of months the account was inactive in the past 12 months.}
  \item{\code{contacts_count_12}}{Number of customer service contacts in the past 12 months.}
  \item{\code{credit_limit}}{Credit limit on the card.}
  \item{\code{revolving_balance}}{Revolving balance on the card.}
  \item{\code{available_credit}}{Unused credit, calculated as \code{credit_limit - revolving_balance}.}
  \item{\code{transaction_amount_12}}{Total transaction amount in the past 12 months.}
  \item{\code{transaction_count_12}}{Total number of transactions in the past 12 months.}
  \item{\code{ratio_amount_Q4_Q1}}{Total transaction amount in the fourth quarter divided by the total transaction amount in the first quarter.}
  \item{\code{ratio_count_Q4_Q1}}{Number of transactions in the fourth quarter divided by the number of transactions in the first quarter.}
  \item{\code{utilization_ratio}}{Proportion of the credit limit in use, calculated as \code{revolving_balance / credit_limit} and rounded to three decimal places.}
  \item{\code{churn}}{Account closure indicator. A factor with levels \code{"yes"} (the account was closed) and \code{"no"} (the account remained active).}
}
}

\details{
The variables \code{education}, \code{marital}, and \code{income} use the factor level \code{"unknown"} for customers whose information is unavailable. These entries are included when counting factor levels and are not counted as missing by \code{is.na()}.
}

\source{
This dataset is originally from https://leaps.analyttica.com/home
}

\references{
Reza Mohammadi (2025). Data Science Foundations and Machine Learning with R: From Data to Decisions. \url{https://book-data-science-r.netlify.app}.
}

\seealso{
\code{\link{churn_mlc}}, \code{\link{churn_tel}}
}

\examples{
data(churn)
str(churn)
table(churn$churn)
}


\keyword{datasets}
