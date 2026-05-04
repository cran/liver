\name{purchase_intention}
\alias{purchase_intention}
\docType{data}
\title{Online Shopper Purchase Intention Data}
\description{
A dataset containing session-level information from an e-commerce website,
including page visit counts, time spent in different page categories,
Google Analytics metrics, visitor characteristics, and a binary outcome
indicating whether the session ended in a purchase. The dataset can be used
to illustrate binary classification, exploratory data analysis, model
comparison, and supervised learning methods in R.
}
\usage{
data(purchase_intention)
}
\format{
A data frame with 12330 observations and 18 variables:
\describe{
  \item{administrative}{Number of administrative pages visited during the session.}
  \item{administrative_duration}{Total time spent on administrative pages during the session.}
  \item{informational}{Number of informational pages visited during the session.}
  \item{informational_duration}{Total time spent on informational pages during the session.}
  \item{product_related}{Number of product-related pages visited during the session.}
  \item{product_related_duration}{Total time spent on product-related pages during the session.}
  \item{bounce_rates}{Average bounce rate associated with the visited pages.}
  \item{exit_rates}{Average exit rate associated with the visited pages.}
  \item{page_values}{Average page value for pages visited before a completed transaction.}
  \item{special_day}{Closeness of the session date to a special shopping day, scaled between 0 and 1.}
  \item{month}{Month of the session.}
  \item{operating_systems}{Visitor operating system, recorded as a categorical factor.}
  \item{browser}{Visitor browser, recorded as a categorical factor.}
  \item{region}{Visitor region, recorded as a categorical factor.}
  \item{traffic_type}{Traffic source type, recorded as a categorical factor.}
  \item{visitor_type}{Visitor type: \code{"New_Visitor"}, \code{"Returning_Visitor"}, or \code{"Other"}.}
  \item{weekend}{Whether the session occurred on a weekend: \code{"no"} or \code{"yes"}.}
  \item{revenue}{Whether the session ended in a purchase: \code{"no"} or \code{"yes"}.}
}
}
\details{
This dataset was obtained from the UCI Machine Learning Repository and renamed
\code{purchase_intention} for inclusion in the \pkg{liver} package. It contains
session-level records from an online shopping website and is well suited for
illustrating modern binary classification problems in which the goal is to
predict whether a browsing session will end in a purchase.

The predictors combine behavioral measures such as page visit counts and time
spent on different types of pages with summary metrics such as
\code{bounce_rates}, \code{exit_rates}, and \code{page_values}, as well as
visitor and session characteristics including \code{month},
\code{visitor_type}, \code{traffic_type}, and \code{weekend}. The outcome
variable \code{revenue} indicates whether the session resulted in a completed
transaction.

The dataset is particularly useful for demonstrating classification workflows
such as partitioning data into training and test sets, fitting logistic
regression, Naive Bayes, k-nearest neighbors, and tree-based models, and
evaluating predictive performance using confusion matrices, ROC curves, and AUC.
}
\source{
UCI Machine Learning Repository:
\url{https://archive.ics.uci.edu/dataset/468/online+shoppers+purchasing+intention+dataset}

Sakar, C. and Kastro, Y. (2018).
\emph{Online Shoppers Purchasing Intention Dataset}.
\doi{10.24432/C5F88Q}
}
\references{
Sakar, C. O., Polat, S. O., Katircioglu, M., and Kastro, Y. (2019).
Real-time prediction of online shoppers' purchasing intention using multilayer perceptron and LSTM recurrent neural networks.
\emph{Neural Computing and Applications}, 31, 6893--6908.
\doi{10.1007/s00521-018-3523-0}

Reza Mohammadi (2025).
\emph{Data Science Foundations and Machine Learning with R: From Data to Decisions}.
\url{https://book-data-science-r.netlify.app}
}

\seealso{ \code{\link[liver]{mortgage}}, 
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
data(purchase_intention)

str(purchase_intention)

summary(purchase_intention)
}

\keyword{datasets}













