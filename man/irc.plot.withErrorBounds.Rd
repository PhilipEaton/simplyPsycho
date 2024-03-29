% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ircPlotErrorBounds.R
\name{irc.plot.withErrorBounds}
\alias{irc.plot.withErrorBounds}
\title{Plot Item Response Curves with error bounds:}
\usage{
irc.plot.withErrorBounds(irc.data.from.booted, qq)
}
\arguments{
\item{irc.data.from.booted}{output from irc.get.Booted() function.}

\item{qq}{Question number for the generated plot.}
}
\value{
IRC plot with error bounds for item number qq.
}
\description{
Given the output from irc.get.Booted() function, create an IRC plot
with error bounds for a specified item.

NOTE ABOUT WARNING MESSAGES: You will likely get a warning like "zero-length
arrow is of indeterminate angle and so skipped". This can be ignored as it
does not impact the plotting.
}
\examples{
# Pull sample data
temp.data <- piql.data.select(simplySampleData, courses = 1, numBlanks.allowed = 0)
data.alpha <- temp.data$data.alpha
data.num <- temp.data$data.num

# Get irc data with default values.
irc.data.booted <- irc.get.Booted(data.alpha,data.num)
# Plot item 2.
irc.plot.withErrorBars(irc.data.booted, 2)
# Plot item 4.
irc.plot.withErrorBounds(irc.data.booted, 4)
}
