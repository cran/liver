\name{one.hot}
\alias{one.hot}
\title{One Hot Encoder}
\description{
One-hot encode categorical columns using base R. Character columns are converted
 to factors, with levels in order of first appearance.
}
\usage{
one.hot(data, cols = "auto", sparsifyNAs = FALSE, naCols = FALSE,
        dropCols = FALSE, dropUnusedLevels = FALSE)
}
\arguments{
  \item{data}{A vector, matrix, \code{data.frame}, or \code{data.table}.}
  \item{cols}{Column names or positive integer column indices to encode.
    The default, \code{"auto"}, selects all unordered factor columns after
    converting character columns to factors. Explicitly selected columns must
    be factors or character vectors. Use \code{character(0)} or \code{NULL}
    to select no columns.}
  \item{sparsifyNAs}{Logical. If \code{TRUE}, replace missing values in the
    generated level indicators with zeros. Otherwise, retain them as \code{NA}.
    Original columns are not affected.}
  \item{naCols}{Logical. If \code{TRUE}, add a missing-value indicator for each
    selected column containing missing values.}
  \item{dropCols}{Logical. If \code{TRUE}, drop the original encoded columns.}
  \item{dropUnusedLevels}{Logical. If \code{TRUE}, omit indicators for unused
    factor levels. Otherwise, include indicators for all factor levels.}
}
\details{
Level indicators are integer columns named \code{column_level}. They follow
 their original column in factor-level order. When requested, the missing-value
 indicator is named \code{column_NA} and precedes the level indicators.
Ordered factors are excluded by \code{cols = "auto"}, but can be selected
 explicitly. Original column order and row order are preserved. Input column
 names must be unique and non-empty. Encoding that would create duplicate
 column names produces an error; rename the relevant columns or factor levels.

The encoding itself requires no additional packages. The \pkg{data.table}
 package is required only when the input is a \code{data.table}.
}
\value{
A \code{data.frame}, or a \code{data.table} when the input is a
 \code{data.table}. Vector and matrix inputs return a \code{data.frame};
 a vector is treated as a single column named \code{data}.
The input object is not modified.
}

\author{Reza Mohammadi \email{a.mohammadi@uva.nl}}

\seealso{\code{\link[liver]{scaler}}}

\examples{
data(risk)
str(risk)
risk_one_hot <- one.hot(risk, cols = "auto")
str(risk_one_hot)

# Missing values and unused factor levels
example_data <- data.frame(
    color = factor(c("red", NA, "blue"), levels = c("blue", "green", "red"))
)
one.hot(example_data, sparsifyNAs = TRUE, naCols = TRUE,
        dropCols = TRUE, dropUnusedLevels = TRUE)
}
\keyword{data preprocessing}
