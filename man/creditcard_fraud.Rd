\name{creditcard_fraud}
\alias{creditcard_fraud}
\docType{data}
\title{Credit Card Transactions for Fraud Detection}
\description{
A dataset containing credit card transactions for illustrating fraud detection
and class imbalance in binary classification. The data include anonymized
predictors derived from a principal component analysis, together with transaction
time, transaction amount, and a binary fraud indicator.
}
\usage{
data(creditcard_fraud)
}

\format{
A data frame with 10,000 rows and 31 variables. Each row represents one credit card transaction.
\itemize{
  \item \code{Time} gives the number of seconds since the first transaction in the original dataset.
  \item \code{V1} through \code{V28} are the 28 principal component scores derived from the original transaction features. The original features are confidential.
  \item \code{Amount} gives the transaction amount.
  \item \code{Class} indicates whether the transaction was genuine (\code{0}) or fraudulent (\code{1}).
}
}

\details{
This dataset is a teaching subset derived from the original Credit Card Fraud
Detection dataset available on Kaggle. The original dataset is highly imbalanced.
For inclusion in the \pkg{liver} package, we created a smaller subset with 10000
observations that retains all fraud cases and a random sample of non-fraud cases.
This version is intended for illustrating class imbalance, resampling strategies,
and model evaluation in binary classification.
}
\source{
\url{https://www.kaggle.com/datasets/mlg-ulb/creditcardfraud}
}
\references{
Andrea Dal Pozzolo, Olivier Caelen, Reid A. Johnson, and Gianluca Bontempi
(2015). Calibrating Probability with Undersampling for Unbalanced Classification.
In \emph{2015 IEEE Symposium Series on Computational Intelligence}.

Reza Mohammadi (2025). \emph{Data Science Foundations and Machine Learning with R: From Data to Decisions}. \url{https://book-data-science-r.netlify.app}.
}

\seealso{ 
\code{\link[liver]{mortgage}}, 
          \code{\link[liver]{bank}}, 
          \code{\link[liver]{churn_mlc}}, 
          \code{\link[liver]{churn}}, 
          \code{\link[liver]{churn_tel}}, 
          \code{\link[liver]{adult}}, 
          \code{\link[liver]{cereal}}, 
          \code{\link[liver]{advertising}}, 
          \code{\link[liver]{marketing}}, 
          \code{\link[liver]{drug}}, 
          \code{\link[liver]{house}}, 
          \code{\link[liver]{house_price}}, 
          \code{\link[liver]{red_wines}}, 
          \code{\link[liver]{white_wines}}, 
          \code{\link[liver]{insurance}}, 
          \code{\link[liver]{caravan}}, 
          \code{\link[liver]{loan}}
          }

\examples{
data(creditcard_fraud)
str(creditcard_fraud)

table(creditcard_fraud$Class)
}
\keyword{datasets}













