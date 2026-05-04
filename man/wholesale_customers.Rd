\name{wholesale_customers}
\alias{wholesale_customers}
\docType{data}
\title{Wholesale Customer Spending Data}
\description{
A dataset containing annual spending information for clients of a wholesale
distributor, along with the customer's sales channel and geographic region.
The dataset can be used to illustrate customer segmentation, clustering,
exploratory data analysis, and unsupervised learning methods in R.
}
\usage{
data(wholesale_customers)
}
\format{
A data frame with 440 observations and 8 variables:
\describe{
  \item{fresh}{Annual spending on fresh products (in monetary units).}
  \item{milk}{Annual spending on milk products (in monetary units).}
  \item{grocery}{Annual spending on grocery products (in monetary units).}
  \item{frozen}{Annual spending on frozen products (in monetary units).}
  \item{detergents_paper}{Annual spending on detergents and paper products (in monetary units).}
  \item{delicassen}{Annual spending on delicatessen products (in monetary units).}
  \item{channel}{Customer sales channel: \code{"Horeca"} or \code{"Retail"}.}
  \item{region}{Customer region: \code{"Lisbon"}, \code{"Oporto"}, or \code{"Other"}.}
}
}
\details{
This dataset was obtained from the UCI Machine Learning Repository and renamed
\code{wholesale_customers} for inclusion in the \pkg{liver} package. It refers
to clients of a wholesale distributor and records their annual spending in
several product categories. The dataset is well suited for illustrating methods
for clustering, customer profiling, and multivariate data exploration.

In clustering applications, the numerical spending variables are typically used
to define the clusters, while \code{channel} and \code{region} can be used
afterward to help interpret the resulting customer groups.
}
\source{
UCI Machine Learning Repository:
\url{https://archive.ics.uci.edu/dataset/292/wholesale+customers}
}

\references{
B. Jaya Lakshmi, K. B. Madhuri, and M. Shashi (2017).
An Efficient Algorithm for Density Based Subspace Clustering with Dynamic Parameter Setting.
\emph{International Journal of Information Technology and Computer Science},
9(6), 27--33.
\doi{10.5815/ijitcs.2017.06.04}

Reza Mohammadi (2025). Data Science Foundations and Machine Learning with R: From Data to Decisions. \url{https://book-data-science-r.netlify.app}.
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
data(wholesale_customers)

str(wholesale_customers)

summary(wholesale_customers)
}

\keyword{datasets}













