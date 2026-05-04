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
A data frame with 10000 observations and 31 variables:
\describe{
  \item{Time}{Seconds elapsed between each transaction and the first transaction in the dataset.}
  \item{V1}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V2}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V3}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V4}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V5}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V6}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V7}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V8}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V9}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V10}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V11}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V12}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V13}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V14}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V15}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V16}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V17}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V18}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V19}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V20}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V21}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V22}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V23}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V24}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V25}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V26}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V27}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{V28}{Anonymized predictor obtained from a PCA transformation of the original variables.}
  \item{Amount}{Transaction amount.}
  \item{Class}{Fraud indicator: \code{0} for non-fraudulent transactions and \code{1} for fraudulent transactions.}
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













