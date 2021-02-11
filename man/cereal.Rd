\name{cereal}
\alias{cereal}
\docType{data}
\title{ Cereal data set }

\description{ 
This dataset contains nutrition information for \eqn{77} breakfast cereals and includes \eqn{16} variables. The "\code{reating}" column is our target as a rating of the cereals (Possibly from Consumer Reports?).   
}

\usage{ data( cereal ) }

\format{ 

The \code{cereal} dataset, as a data frame, contains \eqn{77} rows (breakfast cereals) and \eqn{16} columns (variables/features). The \eqn{16} variables are:
 \itemize{
\item \code{name}: Name of cereal.
\item \code{manuf}: Manufacturer of cereal:
    \itemize{
        \item \code{A}: American Home Food Products;
        \item \code{G}: General Mills;
        \item \code{K}: Kelloggs;
        \item \code{N}: Nabisco;
        \item \code{P}: Post;
        \item \code{Q}: Quaker Oats;
        \item \code{R}: Ralston Purina;
    }
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

\details{
These datasets have been gathered and cleaned up by Petra Isenberg, Pierre Dragicevic and Yvonne Jansen. The original source can be found:
\url{https://perso.telecom-paristech.fr/eagan/class/igr204/datasets}
}


\seealso{ \code{\link{adult}}, \code{\link{risk}}, \code{\link{churn}}, \code{\link{churnTel}}, \code{\link{bank}}, \code{\link{advertising}}, \code{\link{marketing}}, \code{\link{insurance}}, \code{\link{housePrice}}, \code{\link{house}} }

\examples{
data( cereal )

str( cereal )
}

\keyword{ datasets }
