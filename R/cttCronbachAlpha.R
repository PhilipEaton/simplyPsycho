#' CTT Cronbach's Alpha:
#'
#' @description Calculate Cronbach's Alpha for an assessment from sample data..
#'
#' @param data An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
#' where nS is the number of students in the sample and nQ is the number of questions.
#'
#' @param booted Logical (default = FALSE). FALSE means no bootstrapping will be
#' performed. TRUE turns on the bootstrapping feature.
#'
#' @param nRuns Number of random samples to use in the bootstrapping (default = 100).
#' For publications it is recommended that 10,000 runs be performed since sample
#' error goes as 1/sqrt(nRuns).
#'
#' @return When booted = FALSE (the default setting) then the straight calculated
#' value will be returned. If booted = TRUE, the function will output the mean and
#' standard deviation Cronbach's Alpha values calculated from nRuns randomly sampled
#' with replacement data sets from the given sample.
#'
#' @export
#'
#' @examples
#' # Pull in PIQL data from AWS and get some course data.
#' PIQLdata <- pullPQLdata()
#' temp.piql.data <- PIQLdata$courses
#' data.num <- temp.data$data.num
#'
#' # Straight Cronbach's Alpha
#' cttCronbachAlpha(data.num)
#'
#' # Booted using 100 random samples (default)
#' cttCronbachAlpha(data.num, booted = TRUE)
#'
#' # Booted using 1000 random samples by manually setting nRuns
#' cttCronbachAlpha(data.num, booted = TRUE, nRuns = 1000)
cttCronbachAlpha <- function(data, booted = FALSE, nRuns = 100){
  if (booted == FALSE) {
    return( cronbachAlpha(data) )
  }

  if (booted == TRUE) {
    # Get number of students in sample
    nS <- nrow(data)
    cronAlpha.booted <- array(NA, dim = c(nRuns, 1))
    for (ii in 1:nRuns) {
      # Draw a sample (with replacement) from the full sample
      ## that is the same size as the full sample.
      random.sample <- data[sample(1:nS,nS,replace = TRUE),]
      # Calculate and store disc
      cronAlpha.booted[ii] <-  cronbachAlpha(random.sample)
    }

    # Means for each item across all runs
    cronbach.alpha.mn <- mean(cronAlpha.booted)
    # St. Dev. for each item across all runs
    cronbach.alpha.sd <- sd(cronAlpha.booted)
    # Format return
    thing.return <- data.frame(alphaMean = cronbach.alpha.mn, AlphaStDev = cronbach.alpha.sd)
    return(thing.return)
  }

}
