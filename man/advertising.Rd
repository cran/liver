\name{advertising}
\alias{advertising}
\docType{data}
\title{ advertising data set }

\description{ 
the dataset is from an anonymous organisation's social media ad campaign. the advertising dataset contains \eqn{11} features and \eqn{1143} records. 
}

\usage{ data(advertising) }

\format{ 
the \code{advertising} dataset, as a data frame, contains \eqn{1143} rows and \eqn{11} columns (variables/features). the \eqn{11} variables are:
 \itemize{

\item \code{ad.id}: an unique ID for each ad.
\item \code{xyz.campaign.id}: an ID associated with each ad campaign of XYZ company.
\item \code{fb.campaign.id}: an ID associated with how Facebook tracks each campaign.
\item \code{age}: age of the person to whom the ad is shown.
\item \code{gender}: gender of the person to whim the add is shown.
\item \code{interest}: a code specifying the category to which the person's interest belongs (interests are as mentioned in the person's Facebook public profile).
\item \code{impressions}: the number of times the ad was shown.
\item \code{clicks}: number of clicks on for that ad.
\item \code{spend}: amount paid by company xyz to Facebook, to show that ad.
\item \code{conversion}: total number of people who enquired about the product after seeing the ad.
\item \code{approved}: total number of people who bought the product after seeing the ad.
  }
}

\details{
For more information related to the dataset see:

\verb{https://www.kaggle.com/loveall/clicks-conversion-tracking}
}

\seealso{ \code{\link[liver]{adult}}, \code{\link[liver]{risk}}, \code{\link[liver]{churn}}, \code{\link[liver]{churnTel}}, \code{\link[liver]{bank}}, \code{\link[liver]{marketing}}, \code{\link[liver]{insurance}}, \code{\link[liver]{cereal}}, \code{\link[liver]{housePrice}}, \code{\link[liver]{house}} }

\examples{
data(advertising)

str(advertising)
}

\keyword{ datasets }
