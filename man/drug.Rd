\name{drug}
\alias{drug}
\docType{data}
\title{ drug data set }

\description{ 
synthetically generated dataset of \eqn{200} patients includes their age, sodium-to-potassium (Na/K) ratio, and the prescribed drug type.
}

\usage{ data(drug) }

\format{ 

the \code{drug} dataset, as a data frame, contains \eqn{200} rows (customers) and \eqn{3} columns (variables/features). the \eqn{3} variables are:
 \itemize{

\item \code{age}: age of patients. 
\item \code{ratio}: sodium-to-potassium (Na/K) ratio.
\item \code{type}: the prescribed drug type in three levels (A, B, and C).
  }
}

\references{
Reza Mohammadi (2025). Data Science Foundations and Machine Learning with R: From Data to Decisions. \url{https://book-data-science-r.netlify.app}.
}

\seealso{ \code{\link[liver]{adult}}, \code{\link[liver]{risk}}, \code{\link[liver]{churn}}, \code{\link[liver]{churnTel}}, \code{\link[liver]{bank}}, \code{\link[liver]{advertising}}, \code{\link[liver]{marketing}}, \code{\link[liver]{cereal}}, \code{\link[liver]{housePrice}}, \code{\link[liver]{house}} }

\examples{
data(drug)
str(drug)
}

\keyword{ datasets }
