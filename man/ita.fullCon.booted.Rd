% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/itaFullControlBooted.R
\name{ita.fullCon.booted}
\alias{ita.fullCon.booted}
\title{Item Tree Analysis - Full Control with bootstrapping}
\usage{
ita.fullCon.booted(
  data,
  perc = 0.2,
  method = 1,
  retain.perc = 0.9,
  nRuns = 10,
  quite = FALSE
)
}
\arguments{
\item{data}{An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
where nS is the number of students in the sample and nQ is the number of questions.}

\item{perc}{(Default = 0.20) Percentage of either the number of students or the
max number of contradictions to be retained in the model.}

\item{method}{(Defait = 1) Method 1 is based on the number of students and will be
the same for every bootstrapped model. Method 2 is based on the maximum number
on contradictions in the initial model and can vary a little between random samples.}

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
Performs an ITA where the user gets to control the max amount of
contractions between a causal pairing to be retained in the model.

This file does item tree analysis with bootstrapping for error estimation:
 Relaxed the code to so that models will always
 1) includes all of the items on the instrument, and
 2) select the model with the best diff from those models.

Not really recommend for publications boasting quantitative
robustness. This method is EXTREMELY adhoc and should only be used for
qualitative analysis purposes
}
\examples{
# Get PIQL data
temp.data <- piql.data.select(simplySampleData, courses = 1, numBlanks.allowed = 0, MCMR.grading = "Selected")
data.num.MCMR <- temp.data$data.num
# ITA with bootstrapping
ita.data.fc.booted <- ita.fullCon.booted(data.num.MCMR, type = 3, nRuns = 3)

## PLOTTIING
# Set labels
labs <- c("Plants", "Miner", "Fish", "Hooke's Law", "Ferris",
          "Inv. g", "JogAB", "SphBottle", "mkp", "Slide", "Odometer",
          "Squareness", "Olive Oil", "Ch.Sph.", "Int. E", "Q&N", "Bhutan-A",
          "Bhutan-C", "Bhutan-D", "Work-D", "Work-G", "EField", "Delta v")
# Plot bootstrapped model
ita.Plot(ita.data.fc.booted, labs = labs)
}
