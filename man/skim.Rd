\name{skim}
\alias{skim}
\alias{print.liver_skim}
\title{Summarize Variables in a Data Frame or Matrix}

\description{
\code{skim()} displays the dimensions of the input, the number of selected
variables of each type, and a separate summary table for each type.
}

\usage{
skim(data, hist = TRUE, ...)

\method{print}{liver_skim}(x, ..., digits = 3L)
}

\arguments{
  \item{data}{A data frame or matrix. For a grouped data frame, summaries
  are computed within each group.}
  \item{hist}{A logical value. If \code{TRUE} (the default), include a
  five-bin inline histogram for each numeric variable.}
  \item{...}{For \code{skim()}, columns to summarize, specified by name,
  position, or range. By default, all columns except grouping variables are
  summarized. Selection helpers such as \code{starts_with()} are available
  when \pkg{tidyselect} is installed. The print method ignores additional
  arguments.}
  \item{x}{An object returned by \code{skim()}.}
  \item{digits}{An integer from 1 to 15 controlling the precision of
  printed numeric summaries. The default is 3.}
}

\details{
Numeric summaries include the mean, standard deviation, minimum, quartiles,
and maximum. The histogram uses finite values. Factor summaries report
whether the factor is ordered, the number of observed levels, and counts
for up to four levels. Level labels in the counts are abbreviated to three
characters.
}

\value{
\code{skim()} returns a data frame of class \code{liver_skim}, with one row
per selected variable, or one row per variable and group for grouped data.
The columns \code{skim_type} and \code{skim_variable} identify the variable
type and name. The columns \code{n_missing} and \code{complete_rate} give
the number of missing values and the proportion of observations that are
nonmissing, respectively. Type-specific column names include a prefix,
such as \code{numeric.mean} or \code{factor.top_counts}.

The print method displays the summary and returns \code{x} invisibly.
Rounding affects only the printed output.
}

\author{Reza Mohammadi \email{a.mohammadi@uva.nl}}

\seealso{\code{\link[base]{summary}}}

\examples{
data(risk)
skim(risk)
skim(risk, hist = FALSE, age, income)
}

\keyword{univar}
