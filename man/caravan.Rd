\name{caravan}
\alias{caravan}
\docType{data}
\title{Caravan insurance data set}

\description{
The contains \eqn{5822} customer records from an insurance company, each described by \eqn{86} variables. These include \eqn{43} sociodemographic features based on zip codes and \eqn{43} indicators of product ownership. The final variable, \code{Purchase}, indicates whether a customer bought a caravan insurance policy. Collected for the CoIL 2000 Challenge, the data was designed to address the question: \emph{Can you predict who would be interested in buying a caravan insurance policy and explain why?} 
}

\usage{data(caravan)}

\format{A data frame with \eqn{5822} observations (rows) and \eqn{86} features (columns).}

\details{
For more information related to the dataset see  \cr
\url{https://www.kaggle.com/datasets/uciml/caravan-insurance-challenge}
}

\source{The data was supplied by Sentient Machine Research: \url{https://www.smr.nl}}

\references{
P. van der Putten and M. van Someren (eds) . CoIL Challenge 2000: The Insurance Company Case.  Published by Sentient Machine Research, Amsterdam. Also a Leiden Institute of Advanced Computer Science Technical Report 2000-09. June 22, 2000.  \cr
James, G., Witten, D., Hastie, T., and Tibshirani, R. (2013). An Introduction to Statistical Learning with applications in R, \url{https://www.statlearning.com}, \emph{Springer-Verlag}.

Reza Mohammadi (2025). Data Science Foundations and Machine Learning with R: From Data to Decisions. \url{https://book-data-science-r.netlify.app}.
}
  
\seealso{ \code{\link[liver]{bank}}, 
          \code{\link[liver]{churn}}, 
          \code{\link[liver]{churnCredit}}, 
          \code{\link[liver]{churnTel}}, 
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
          \code{\link[liver]{fertilizer}}, 
          \code{\link[liver]{corona}}
          }

\examples{
data(caravan)
str(caravan)
}

\keyword{datasets}

