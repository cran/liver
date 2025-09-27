\name{insurance}
\alias{insurance}
\docType{data}
\title{ insurance data set }

\description{ 
the insurance dataset contains \eqn{7} features and \eqn{1338} records. the target feature is \emph{charge} and the remaining 6 variables are predictors.
}

\usage{ data(insurance) }

\format{ 

the \code{insurance} dataset, as a data frame, contains \eqn{1338} rows (customers) and \eqn{7} columns (variables/features). the \eqn{7} variables are:
 \itemize{

\item \code{age}: age of primary beneficiary.
\item \code{bmi}: body mass index, providing an understanding of body, weights that are relatively high or low relative to height, objective index of body weight (kg / m ^ 2) using the ratio of height to weight, ideally 18.5 to 24.9.
\item \code{children}: Number of children covered by health insurance / Number of dependents.
\item \code{smoker}: Smoking as a factor with 2 levels, yes, no.
\item \code{gender}: insurance contractor gender, female, male.
\item \code{region}: the beneficiary's residential area in the US, northeast, southeast, southwest, northwest.
\item \code{charge}: individual medical costs billed by health insurance.
  }
}

\details{
For more information related to the dataset see: \cr
\url{https://www.kaggle.com/mirichoi0218/insurance}
}

\references{
Brett Lantz (2019). Machine Learning with R: Expert techniques for predictive modeling. \emph{Packt Publishing Ltd}.
}

\seealso{ \code{\link[liver]{adult}}, \code{\link[liver]{risk}}, \code{\link[liver]{churn}}, \code{\link[liver]{churnTel}}, \code{\link[liver]{bank}}, \code{\link[liver]{advertising}}, \code{\link[liver]{marketing}}, \code{\link[liver]{cereal}}, \code{\link[liver]{housePrice}}, \code{\link[liver]{house}} }

\examples{
data(insurance)
str(insurance)
}

\keyword{ datasets }
