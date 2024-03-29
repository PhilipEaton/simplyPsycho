% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/itaStrict.R
\name{ita.strict}
\alias{ita.strict}
\title{Item Tree Analysis - Strictly Performed}
\usage{
ita.strict(data, type = 1)
}
\arguments{
\item{data}{An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
where nS is the number of students in the sample and nQ is the number of questions.}

\item{type}{(Default = 1) Type of ITA to be performed.

Type = 1 - Original IITA

Type = 2 - Corrected IITA

Type = 3 - Minimized Corrected IITA}
}
\value{
ITA map to be plotted using itaPlot() function.

     $model Model found from analysis.

     $diff Diff for the model.

     $errorRate Error Rate for the model.
}
\description{
Performs ITA using strict procedures.
This function is set up to consider every sensible tree by first considering
all possible connections in the assessment, then iteratively removing the worst
offending connection, one at a time, until no connections are left.
The returned model is the one which returns the smallest difference between
the estimated and original contradiction matrices.

NOTE: The generally means a handful of questions get likely be removed entirely
from the tree due to high diff and error rates in the models that include them.

SUGGESTION: A less strict version of this function is itaRelaxed and itxVeryRelaxed.
}
\examples{
# Pull sample data
temp.data <- piql.data.select(simplySampleData, courses = 1, numBlanks.allowed = 0, MCMR.grading = "Selected")
data.alpha.MCMR <- temp.data$data.alpha
data.num.MCMR <- temp.data$data.num
# Check number of student removed. Should be less than 10\%.
temp.data$nS.details

# Original ITA
ita.data.1 <- ita.strict(data.num.MCMR,type = 1)
ita.data.1$model
ita.data.1$diff
ita.data.1$errorRate

# Corrected ITA
ita.data.2 <- ita.strict(data.num.MCMR,type = 2)
ita.data.2$model
ita.data.2$diff
ita.data.2$errorRate

# Minimized Corredted ITA
ita.data.3 <- ita.strict(data.num.MCMR,type = 3)
ita.data.3$model
ita.data.3$diff
ita.data.3$errorRate

## PLOTTIING
# Set labels
labs <- c("Plants", "Miner", "Fish", "Hooke's Law", "Ferris",
"Inv. g", "JogAB", "SphBottle", "mkp", "Slide", "Odometer",
"Squareness", "Olive Oil", "Ch.Sph.", "Int. E", "Q&N", "Bhutan-A",
"Bhutan-C", "Bhutan-D", "Work-D", "Work-G", "EField", "Delta v")

# and plot
irt.Plot(ita.data.1$model, labs = labs)
irt.Plot(ita.data.2$model, labs = labs)
irt.Plot(ita.data.3$model, labs = labs)
}
