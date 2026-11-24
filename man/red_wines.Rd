\name{red_wines}
\alias{red_wines}
\docType{data}
\title{ Red wines data set }

\description{ 
the red_wines datasets are related to red variants of the Portuguese "Vinho Verde" wine. Due to privacy and logistic issues, only physicochemical (inputs) and sensory (the output) variables are available (e.g. there is no data about grape types, wine brand, wine selling price, etc.).

the dataset can be viewed as classification or regression tasks. the classes are ordered and not balanced (e.g. there are many more normal wines than excellent or poor ones). Outlier detection algorithms could be used to detect the few excellent or poor wines. Also, we are not sure if all input variables are relevant. So it could be interesting to test feature selection methods.
}

\usage{ data(red_wines) }





\format{
A data frame with 1,599 rows and 12 variables. Each row represents one sample of Portuguese red Vinho Verde wine. The first 11 variables are physicochemical measurements, and \code{quality} is the sensory outcome.
\describe{
  \item{\code{fixed_acidity}}{Fixed acidity, expressed as grams of tartaric acid per liter.}
  \item{\code{volatile_acidity}}{Volatile acidity, expressed as grams of acetic acid per liter.}
  \item{\code{citric_acid}}{Citric acid concentration in grams per liter.}
  \item{\code{residual_sugar}}{Sugar remaining after fermentation, measured in grams per liter.}
  \item{\code{chlorides}}{Chloride concentration, expressed as grams of sodium chloride per liter.}
  \item{\code{free_sulfur_dioxide}}{Concentration of free sulfur dioxide in milligrams per liter.}
  \item{\code{total_sulfur_dioxide}}{Combined concentration of free and bound sulfur dioxide in milligrams per liter.}
  \item{\code{density}}{Wine density in grams per milliliter.}
  \item{\code{pH}}{Wine pH. Lower values indicate greater acidity.}
  \item{\code{sulphates}}{Sulfate concentration, expressed as grams of potassium sulfate per liter.}
  \item{\code{alcohol}}{Alcohol content by volume, expressed as a percentage.}
  \item{\code{quality}}{Integer quality score. At least three assessors rated each wine in a blind tasting on a scale from 0 (very poor) to 10 (excellent). The recorded score is the median of their ratings. Scores in this dataset range from 3 to 8.}
}
}

\details{
For more information related to the dataset see the UCI Machine Learning Repository: \cr
\url{https://archive.ics.uci.edu/dataset/186/wine+quality}
}

\source{
This dataset comes from the UCI repository of machine learning databases: \cr
\url{https://archive.ics.uci.edu/dataset/186/wine+quality}
}

\references{
Cortez, P., Cerdeira, A., Almeida, F., Matos, T., and Reis, J. (2009). Modeling wine preferences by data mining from physicochemical properties. \emph{Decision support systems}, 47(4), 547-553.

Reza Mohammadi (2025). Data Science Foundations and Machine Learning with R: From Data to Decisions. \url{https://book-data-science-r.netlify.app}.
}

\seealso{ \code{\link[liver]{white_wines}} }

\examples{
data(red_wines)
str(red_wines)
}

\keyword{ datasets }
