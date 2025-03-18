\name{conf.mat.plot}
\alias{conf.mat.plot}

\title{	Plot Confusion Matrix }
\description{ Plot a Confusion Matrix. }

\usage{
conf.mat.plot(pred, actual, cutoff = 0.5, reference = NULL, conf.level = 0, 
              margin = 1, color = c("#ff83a8", "#83ff9b"), ...)
}

\arguments{
    \item{pred  }{a vector of estimated values. }
    \item{actual}{a vector of actual values. }
    \item{cutoff}{cutoff value for the case that \code{pred} is vector of probabilites. }  
    \item{reference}{a factor of classes to be used as the true results. }
    \item{conf.level}{confidence level used for the confidence rings on the odds ratios. Must be a single nonnegative number less than 1; if set to 0 (the default), confidence rings are suppressed. }  
    \item{margin}{a numeric vector with the margins to equate. Must be one of 1 (the default), 2, or c(1, 2), which corresponds to standardizing the row, column, or both margins in each 2 by 2 table. Only used if std equals "margins". }  
    \item{color}{a vector of length 2 specifying the colors to use for the smaller and larger diagonals of each 2 by 2 table. }  
    \item{\dots }{options to be passed to \code{fourfoldplot}. }
}

\author{ Reza Mohammadi \email{a.mohammadi@uva.nl} and Kevin Burke \email{kevin.burke@ul.ie} }

\seealso{ \code{\link[liver]{conf.mat}} }

\examples{
pred   = c("no", "yes", "yes", "no", "no", "yes", "no", "no")
actual = c("yes", "no", "yes", "no", "no", "no", "yes", "yes")

conf.mat.plot(pred, actual)
}

\keyword{parameter learning}

