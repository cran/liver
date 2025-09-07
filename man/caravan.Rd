\name{caravan}
\alias{caravan}
\docType{data}
\title{Caravan insurance data set}

\description{
The contains \eqn{5822} customer records from an insurance company, each described by \eqn{86} variables. These include \eqn{43} sociodemographic features based on zip codes and \eqn{43} indicators of product ownership. The final variable, \code{Purchase}, indicates whether a customer bought a caravan insurance policy. Collected for the CoIL 2000 Challenge, the data was designed to address the question: \emph{Can you predict who would be interested in buying a caravan insurance policy and explain why?} Further variable details are available at \url{http://www.liacs.nl/~putten/library/cc2000/data.html}.
}

\usage{data(caravan)}

\format{A data frame with \eqn{5822} observations (rows) and \eqn{86} features (columns).}
  
\source{The data was supplied by Sentient Machine Research: \verb{https://www.smr.nl}}

\references{
P. van der Putten and M. van Someren (eds) . CoIL Challenge 2000: The Insurance Company Case.  Published by Sentient Machine Research, Amsterdam. Also a Leiden Institute of Advanced Computer Science Technical Report 2000-09. June 22, 2000. \url{http://www.liacs.nl/~putten/library/cc2000}.

James, G., Witten, D., Hastie, T., and Tibshirani, R. (2013). An Introduction to Statistical Learning with applications in R, \url{https://www.statlearning.com}, \emph{Springer-Verlag}.
}
  
\seealso{
\code{\link[liver]{adult}}, \code{\link[liver]{risk}}, \code{\link[liver]{churn}}, \code{\link[liver]{churnTel}}, \code{\link[liver]{bank}}, \code{\link[liver]{advertising}}, \code{\link[liver]{marketing}}, \code{\link[liver]{insurance}}, \code{\link[liver]{cereal}}, \code{\link[liver]{housePrice}}, \code{\link[liver]{house}}
}

\examples{
data(caravan)

str(caravan)
}

\keyword{datasets}

