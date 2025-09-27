\name{bank}
\alias{bank}

\docType{data}

\title{ Bank marketing data set }

\description{ 
the data is related to direct marketing campaigns of a Portuguese banking institution. the marketing campaigns were based on phone calls. Often, more than one contact to the same client was required, in order to access if the product (bank term deposit) would be (or not) subscribed. the classification goal is to predict if the client will subscribe a term deposit (variable deposit).
}

\usage{ data(bank) }

\format{ 

the \code{bank} dataset, as a data frame, contains \eqn{4521} rows (customers) and \eqn{17} columns (variables/features). the \eqn{17} variables are:

Bank client data:
\itemize{
\item \code{age}: numeric.
\item \code{job}: type of job; categorical: "admin.", "unknown", "unemployed", "management", "housemaid", "entrepreneur", "student", "blue-collar, "self-employed", "retired", "technician", "services".
\item \code{marital}: marital status; categorical: "married", "divorced", "single"; note: "divorced" means divorced or widowed.
\item \code{education}: categorical: "secondary", "primary", "tertiary", "unknown".
\item \code{default}: has credit in default?; binary: "yes","no".
\item \code{balance}: average yearly balance, in euros; numeric.
\item \code{housing}: has housing loan? binary: "yes", "no".
\item \code{loan}: has personal loan? binary: "yes", "no".
}

Related with the last contact of the current campaign:
\itemize{
\item \code{contact}: contact: contact communication type; categorical: "unknown","telephone","cellular". 
\item \code{day}: last contact day of the month; numeric.
\item \code{month}: last contact month of year; categorical: "jan", "feb", "mar", ..., "nov", "dec".
\item \code{duration}: last contact duration, in seconds; numeric.
}

Other attributes:
\itemize{
\item \code{campaign}: number of contacts performed during this campaign and for this client; numeric, includes last contact.
\item \code{pdays}: number of days that passed by after the client was last contacted from a previous campaign; numeric, -1 means client was not previously contacted.
\item \code{previous}: number of contacts performed before this campaign and for this client; numeric.
\item \code{poutcome}: outcome of the previous marketing campaign; categorical: "success", "failure", "unknown", "other".
}

Target variable:
\itemize{
\item \code{deposit}: Indicator of whether the client subscribed a term deposit; binary: "yes" or "no".
}
}

\details{
For more information related to the dataset see:  \cr
\url{http://archive.ics.uci.edu/ml/datasets/Bank+Marketing}
}

\references{
Moro, S., Laureano, R. and Cortez, P. (2011) Using Data Mining for Bank Direct Marketing: An Application of the CRISP-DM Methodology. In P. Novais et al. (Eds.), Proceedings of the European Simulation and Modelling Conference.
}

\seealso{ \code{\link[liver]{adult}}, \code{\link[liver]{risk}}, \code{\link[liver]{churn}}, \code{\link[liver]{churnTel}}, \code{\link[liver]{advertising}}, \code{\link[liver]{marketing}}, \code{\link[liver]{insurance}}, \code{\link[liver]{cereal}}, \code{\link[liver]{housePrice}}, \code{\link[liver]{house}} }

\examples{
data(bank)
str(bank)
}

\keyword{ datasets }
