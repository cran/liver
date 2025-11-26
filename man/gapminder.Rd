\name{gapminder}
\alias{gapminder}
\encoding{UTF-8}

\docType{data}

\title{Gapminder Data on Global Health, Income, and Population}

\description{
The \emph{gapminder} dataset provides global health, income, and population indicators for \eqn{195} countries over the period \eqn{1950}--\eqn{2019}.
}

\usage{ data(gapminder) }

\format{
The \code{gapminder} dataset, provided as a data frame, contains \eqn{13{,}650} rows and \eqn{8} columns (features) as follows:
\itemize{
  \item \code{country}: Country name.
  \item \code{year}: Calendar year of observation (1950--2019).
  \item \code{gdp}: Gross domestic product (GDP) in USD, based on World Bank data.
  \item \code{life_expectancy}: Average life expectancy at birth (in years).
  \item \code{population}: National population size.
  \item \code{continent}: Continent to which the country belongs.
  \item \code{iso_alpha}: ISO 3166-1 alpha-3 country code.
  \item \code{world_group}: A five-category geopolitical grouping of countries used for visualization, with levels \emph{The West}, \emph{Asia}, \emph{Latin America}, \emph{Africa}, and \emph{Others}.
}
}

\details{
For more information related to the dataset see:  \cr
\url{https://www.gapminder.org/data/documentation/}
}

\source{
This dataset is originally from \url{https://www.gapminder.org/resources/}
}

\references{
Reza Mohammadi (2025). Data Science Foundations and Machine Learning with R: From Data to Decisions. \url{https://book-data-science-r.netlify.app}.
}

\seealso{ \code{\link[liver]{bank}}, 
          \code{\link[liver]{churnCredit}}, 
          \code{\link[liver]{churn}}, 
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
          \code{\link[liver]{caravan}}, 
          \code{\link[liver]{fertilizer}}, 
          \code{\link[liver]{corona}}
          }

\examples{
data(gapminder)

str(gapminder)
}

\keyword{ datasets }
