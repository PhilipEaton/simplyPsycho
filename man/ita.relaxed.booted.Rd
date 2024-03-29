% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/itaRelaxedBooted.R
\name{ita.relaxed.booted}
\alias{ita.relaxed.booted}
\title{Item Tree Analysis - Relaxed with bootstrapping}
\usage{
ita.relaxed.booted(
  data,
  type = 1,
  retain.perc = 0.9,
  nRuns = 10,
  quite = FALSE
)
}
\arguments{
\item{data}{An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
where nS is the number of students in the sample and nQ is the number of questions.}

\item{type}{(Default = 1) Type of ITA to be performed.

Type = 1 - Original IITA

Type = 2 - Corrected IITA

Type = 3 - Minimized Corrected IITA}

\item{retain.perc}{(Default = 0.90) Retain the links that are present in this percentage of
all the bootstrapped runs.}

\item{nRuns}{Number of random samples to use in the bootstrapping (default = 10).
For publications it is recommended that 10,000 runs be performed since sample
error goes as 1/sqrt(nRuns).}
}
\value{
ITA map resulting from bootstrapping to be plotted using itaPlot() function.
}
\description{
Performs a relaxed ITA.
This function is set up to consider every sensible tree by first considering
all possible connections in the assessment, then iteratively removing the worst
offending connection, one at a time, until no connections are left.
The returned model is the one which returns the smallest difference between
the estimated and original contradiction matrices AND retains all of the items
on the assessment.
}
\examples{
# Pull sample data
temp.data <- piql.data.select(simplySampleData, courses = 1, numBlanks.allowed = 0, MCMR.grading = "Selected")
data.num.MCMR <- temp.data$data.num
# ITA with bootstrapping
ita.data.3.booted <- ita.relaxed.booted(data.num.MCMR, type = 3, nRuns = 3)

## PLOTTIING
# Set labels
labs <- c("Plants", "Miner", "Fish", "Hooke's Law", "Ferris",
          "Inv. g", "JogAB", "SphBottle", "mkp", "Slide", "Odometer",
          "Squareness", "Olive Oil", "Ch.Sph.", "Int. E", "Q&N", "Bhutan-A",
          "Bhutan-C", "Bhutan-D", "Work-D", "Work-G", "EField", "Delta v")
# Plot bootstrapped model
ita.Plot(ita.data.3.booted, labs = labs)
}
