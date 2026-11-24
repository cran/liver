\name{bike_demand}
\alias{bike_demand}
\docType{data}
\title{Seoul Bike Sharing Demand Data}
\description{
A dataset containing hourly bike rental demand in Seoul, South Korea, together
with weather conditions, seasonal information, holiday status, and whether the
bike sharing system was operating on that day.
}
\usage{
data(bike_demand)
}
\format{
A data frame with 8760 observations and 14 variables:
\describe{
  \item{\code{date}}{Date of observation.}
  \item{\code{hour}}{Hour of the day, ranging from 0 to 23.}
  \item{\code{temperature}}{Temperature in degrees Celsius.}
  \item{\code{humidity}}{Humidity percentage.}
  \item{\code{wind_speed}}{Wind speed in meters per second.}
  \item{\code{visibility}}{Visibility in units recorded by the source dataset.}
  \item{\code{dew_point_temperature}}{Dew point temperature in degrees Celsius.}
  \item{\code{solar_radiation}}{Solar radiation in megajoules per square meter.}
  \item{\code{rainfall}}{Rainfall in millimeters.}
  \item{\code{snowfall}}{Snowfall in centimeters.}
  \item{\code{season}}{Season of the year: \code{"spring"}, \code{"summer"}, \code{"autumn"}, or \code{"winter"}.}
  \item{\code{holiday}}{Holiday status: \code{"holiday"} or \code{"no holiday"}.}
  \item{\code{functioning_day}}{Whether the bike rental system was operating: \code{"yes"} or \code{"no"}.}
  \item{\code{bike_count}}{Number of rented bikes (target variable).}
}
}
\details{
This dataset was obtained from the UCI Machine Learning Repository and renamed
\code{bike_demand} for inclusion in the \pkg{liver} package. It can be used to
illustrate methods for regression, exploratory data analysis, and predictive
modeling in R.
}
\source{
\url{https://archive.ics.uci.edu/dataset/560/seoul+bike+sharing+demand}
}

\references{
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
data(bike_demand)

str(bike_demand)
summary(bike_demand)
}
\keyword{datasets}













