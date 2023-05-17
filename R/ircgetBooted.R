#' Item Response Curve data generator with bootstrapping for error estimation:
#'
#' @description Calculates the plotting data needed to create item response curves
#' for the items on an assessment using a given sample and bootstrapping. Can be
#' used to plot item response curves with error bars/bounds.
#'
#' @param data.alpha An nS by nQ matrix or data frame of ALPHABETICAL data,
#' where nS is the number of students in the sample and nQ is the number of questions.
#'
#' @param data.num An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
#' where nS is the number of students in the sample and nQ is the number of questions.
#'
#' @param nO The maximum number of options available on the assessment. Default it set to
#' find this value automatically given the response options present in the sample.
#' NOTE: This should almost never need to be changed from its default value!
#'
#' @param nRuns Number of random samples to use in the bootstrapping (default = 10).
#' For publications it is recommended that 10,000 runs be performed since sample
#' error goes as 1/sqrt(nRuns).
#'
#' @return Plotting data for item response curves. To be put into the ircPlotErrorBars()
#' or ircPlotErrorBounds() functions.
#'
#'      $means The mean values from bootstrapping.
#'
#'      $stDevs The standard deviation values from bootstrapping.
#'
#' @export
#'
#' @examples
#' # Pull in PIQL data from AWS and get some course data.
#' PIQLdata <- pullPIQLdata()
#' temp.piql.data <- PIQLdata$courses
#' data.alpha <- temp.data$data.alpha
#' data.num <- temp.data$data.num
#'
#' # Get irc data with default values.
#' irc.data.booted <- irc.get.Booted(data.alpha,data.num)
#' irc.data.booted$means # Mean values
#' irc.data.booted$stDevs # Standard deviation values.
#'
#' # Plot item 2.
#' irc.plot.withErrorBars(irc.data.booted, 2)
#' irc.plot.withErrorBounds(irc.data.booted,2)
#'
#' # Get irc data and plot item 2 with a larger bootstrapping
#' irc.data.booted <- irc.get.Booted(data.alpha,data.num, nRuns = 100)
#' irc.plot.withErrorBars(irc.data.booted, 2)
#' irc.plot.withErrorBounds(irc.data.booted,2)
irc.get.Booted <- function (data.alpha,data.num, nO = NULL, nRuns = 10) {
  nS <- nrow(data.alpha) # Get number of students in sample
  nQ <- ncol(data.alpha) # Get number of questions in sample
  if (is.null(nO)) {
    nO <- length(table(unlist(as.list(data.alpha)))) # Get max number of options
  }
  # set up and run the bootstrap
  irc.booted <- array(NA, dim = c(nRuns, (nQ+1), nQ, nO))
  for (ii in 1:nRuns) {
    # Draw a sample (with replacement) from the full sample
    ## that is the same size as the full sample.
    tempClass <- sample(1:nS,nS,replace = TRUE)
    random.sample.alpha <- data.alpha[tempClass,]
    random.sample.num   <- data.num[tempClass,]
    # Calculate and store irc data
    irc.booted[ii,,,] <-  irc.get(random.sample.alpha,random.sample.num, nO)
  }
  # Find mean while controlling for NAs.
  ## Using just mean can result in summing over the options to be larger than 1
  ## due to situations were one option is NA and the other is 1 in one run and
  ## vice versa in another run. This results in an inflated response rate.
  irc.mn <- array(NA, dim = c((nQ+1), nQ, nO))
  for (ss in 1:(nQ+1)) {
    for (qq in 1:nQ) {
      irc.mn[ss,qq,] <- colSums(irc.booted[,ss,qq,],na.rm=TRUE) / sum(irc.booted[,ss,qq,],na.rm=TRUE)
    }
  }
  # St. Dev. for each item across all runs
  irc.sd <- apply(irc.booted,c(2,3,4),sd, na.rm = TRUE)
  # return the means and StDevs
  thing.return <- list()
  thing.return$means <- irc.mn
  thing.return$stDevs<- irc.sd
  return(thing.return)
}


