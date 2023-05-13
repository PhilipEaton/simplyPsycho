#' CTT Discrimination  with bootstrapping for error estimation:
#'
#' @description Calculate Classical Test Theory item discrimination for the items on
#' an assessment using a given sample.
#'
#' Right now this does the strict top and bottom percentage. The option to switch to
#' percentiles will be added soon.
#'
#' @param data An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
#' where nS is the number of students in the sample and nQ is the number of questions.
#'
#' @param perc Top and bottom percentage to be compared (default = 0.27).
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
#' standard deviation for the CTT item discrimination calculated from nRuns randomly sampled
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
#' cttDisc(data.num)
#'
#' # Booted using 100 random samples (default)
#' cttDisc(data.num, booted = TRUE)
#'
#' # Booted using 1000 random samples by manually setting nRuns
#' cttDisc(data.num, booted = TRUE, nRuns = 1000)
cttDisc <- function (data, perc = 0.27, booted = FALSE, nRuns = 100){
  # Get number of students
  nS <- nrow(data)
  # Get number of questions
  nQ <- ncol(data)
  # Get total scores of the random sample
  total.scores <- rowSums(data)
  if (booted == FALSE) {
  # ctt Discrimination
    # Format return
    thing.return <- t(data.frame(cttDisc = round(colMeans(data[order(total.scores, decreasing = TRUE),][c(1:round(nS*perc,0)),]) -
                                                   colMeans(data[order(total.scores, decreasing = FALSE),][c(1:round(nS*perc,0)),]), 3)))
    if ( is.null(colnames(data.num)) )  {
      colnames(thing.return) <- as.vector(paste0("Q",c(1:nQ)))
    } else {
      colnames(thing.return) <- colnames(data.num)
    }
    return(thing.return)
  }

  if (booted == TRUE) {
    # Build storage array.
    itemDisc.booted <- array(NA, dim = c(nRuns, nQ))
    # Do the runs:
    for (ii in 1:nRuns) {
      # Draw a sample (with replacement) from the full sample
      ## that is the same size as the full sample.
      random.sample <- data[sample(1:nS,nS,replace = TRUE),]
      # Calculate and store disc
      # Get total scores of the random sample
      total.scores <- rowSums(random.sample)
      # Calculate and store disc
      itemDisc.booted[ii,] <-  round(colMeans(random.sample[order(total.scores, decreasing = TRUE),][c(1:round(nS*perc,0)),]) -
                                       colMeans(random.sample[order(total.scores, decreasing = FALSE),][c(1:round(nS*perc,0)),]), 3)
    }

    # Means for each item across all runs
    item.disc.mn <- apply(itemDisc.booted,2,mean)
    # St. Dev. for each item across all runs
    item.disc.sd <- apply(itemDisc.booted,2,sd)
    # Format return
    thing.return <- t(data.frame(cttDisc.Mean = item.disc.mn,
                                 cttDisc.StDev = item.disc.sd))
    if ( is.null(colnames(data.num)) )  {
      colnames(thing.return) <- paste0("Q",c(1:nQ))
    } else {
      colnames(thing.return) <- colnames(data.num)
    }
    return(thing.return)
  }
}
