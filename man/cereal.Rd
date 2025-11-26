\name{cereal}
\alias{cereal}
\docType{data}
\title{ Cereal data set }

\description{ 
This dataset contains nutrition information for \eqn{77} breakfast cereals and includes \eqn{16} variables. the "\code{rating}" column is our target as a rating of the cereals (Possibly from Consumer Reports?).   
}

\usage{ data(cereal) }

\format{ 

the \code{cereal} dataset, as a data frame, contains \eqn{77} rows (breakfast cereals) and \eqn{16} columns (variables/features). the \eqn{16} variables are:
 \itemize{
\item \code{name}: Name of cereal.
\item \code{manuf}: Manufacturer of cereal, coded into seven categories: "A" for American Home Food Products, "G" for General Mills, "K" for Kelloggs, "N" for Nabisco, "P" for Post, "Q" for Quaker Oats, and "R" for Ralston Purina. 
\item \code{type}: cold or hot.
\item \code{calories}: calories per serving.
\item \code{protein}: grams of protein.
\item \code{fat}: grams of fat.
\item \code{sodium}: milligrams of sodium.
\item \code{fiber}: grams of dietary fiber.
\item \code{carbo}: grams of complex carbohydrates.
\item \code{sugars}: grams of sugars.
\item \code{potass}: milligrams of potassium.
\item \code{vitamins}: vitamins and minerals - 0, 25, or 100, indicating the typical percentage of FDA recommended.
\item \code{shelf}: display shelf (1, 2, or 3, counting from the floor).
\item \code{weight}: weight in ounces of one serving.
\item \code{cups}: number of cups in one serving.
\item \code{rating}: a rating of the cereals (Possibly from Consumer Reports?).
  }
}

\source{
The original source can be found:  \cr
\url{https://perso.telecom-paristech.fr/eagan/class/igr204/datasets}
}

\references{
Reza Mohammadi (2025). Data Science Foundations and Machine Learning with R: From Data to Decisions. \url{https://book-data-science-r.netlify.app}.
}

\seealso{ \code{\link[liver]{bank}}, 
          \code{\link[liver]{churn}}, 
          \code{\link[liver]{churnCredit}}, 
          \code{\link[liver]{churnTel}}, 
          \code{\link[liver]{adult}}, 
          \code{\link[liver]{risk}}, 
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
data(cereal)
str(cereal)
}

\keyword{ datasets }
