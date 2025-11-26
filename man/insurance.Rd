\name{insurance}
\alias{insurance}
\docType{data}
\title{ insurance data set }

\description{ 
the insurance dataset contains \eqn{7} features and \eqn{1338} records. the target feature is \emph{charge} and the remaining 6 variables are predictors. This dataset is simulated on the basis of demographic statistics from the US Census Bureau.
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

\source{
This dataset comes from: \cr
\url{https://github.com/stedy/Machine-Learning-with-R-datasets}
}

\references{
Brett Lantz (2019). Machine Learning with R: Expert techniques for predictive modeling. \emph{Packt Publishing Ltd}.

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
          \code{\link[liver]{caravan}}, 
          \code{\link[liver]{fertilizer}}, 
          \code{\link[liver]{corona}}
          }

\examples{
data(insurance)
str(insurance)
}

\keyword{ datasets }
