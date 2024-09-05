\name{skim}
\alias{skim}

\title{	Skim a data frame to get useful summary statistics }

\description{
\code{skim()} provides an overview of a data frame asan alternative to \code{\link[=summary]{summary()}}. This function is a wrapper for the \code{\link[skimr:skim]{skimr::skim()}} function of \code{R} package \code{skimr}. 
}

\usage{
  skim( data, hist = TRUE, ... )
}

\arguments{
  \item{data}{ a data frame or matrix.}
  \item{hist}{ Logical: TRUE (default) to report the histogram of each variable.}
  \item{...}{ columns to select for skimming. the default is to skim all columns.}
}

\author{ Reza Mohammadi \email{a.mohammadi@uva.nl} and Kevin Burke \email{kevin.burke@ul.ie} }

\seealso{ \code{\link[=summary]{summary()}} }

\examples{
data( risk )

skim( risk )
}

\keyword{models}

