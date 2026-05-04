\name{cpu_price}
\alias{cpu_price}
\docType{data}
\title{CPU Specifications and Market Prices}
\description{
A dataset containing detailed specifications, integrated graphics availability,
and market price information for a range of computer processors (CPUs).
It includes hardware characteristics such as core counts, thread counts,
clock speeds, cache size, and thermal design power (TDP), along with price
data. The dataset is suitable for studying price-to-performance trade-offs
across different CPU models.
}
\usage{
data(cpu_price)
}
\format{
A data frame with 45 observations and 12 variables:
\describe{
  \item{model}{The model name of the processor.}
  \item{brand}{The brand of the CPU: \code{"AMD"} or \code{"Intel"}.}
  \item{gpu}{Whether the CPU includes integrated graphics: \code{"yes"} or \code{"no"}.}
  \item{architecture}{The microarchitecture or generation family of the CPU.}
  \item{base_ghz}{The base operating frequency of the CPU in gigahertz.}
  \item{boost_ghz}{The maximum turbo or boost frequency of the CPU in gigahertz.}
  \item{p_cores}{The number of performance cores (P-cores).}
  \item{e_cores}{The number of efficiency cores (E-cores).}
  \item{threads}{The number of logical threads the CPU can execute simultaneously.}
  \item{cache}{The total cache size in megabytes.}
  \item{tdp}{The typical thermal design power (TDP) of the CPU in watts under standard load conditions.}
  \item{price}{The approximate retail market price of the CPU in US dollars.}
}
}
\details{
The dataset was assembled to support exploratory and predictive analyses
of CPU pricing. For example, it can be used in regression models relating
CPU price to processor characteristics such as clock speed, thread count,
graphics support, and brand.
}
\source{
The dataset was collected by the package authors. Hardware specifications
are based on publicly available manufacturer information. Price data was
collected through Google searches during Spring 2026 and reflects approximate
retail market prices at that time.
}

\references{
Reza Mohammadi (2025). Data Science Foundations and Machine Learning with R: From Data to Decisions. \url{https://book-data-science-r.netlify.app}.
}

\seealso{ \code{\link[liver]{bike_demand}}, 
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
data(cpu_price)

str(cpu_price)
summary(cpu_price)
}
\keyword{datasets}













