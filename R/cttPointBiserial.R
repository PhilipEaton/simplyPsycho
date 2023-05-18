#' CTT Point-biserial:
#'
#' @description Calculates the point-biserial correlation between the total score
#' (with focus item removed) and the focus item. This gives a measure if the items
#' on the assessment are all measuring a single similar latent trait. If the point-biserial
#' is poor, then this could be an indication that the assessment is multidimensional.
#'
#' @param booted Logical (default = FALSE). FALSE means no bootstrapping will be
#' performed. TRUE turns on the bootstrapping feature.
#'
#' @param nRuns Number of random samples to use in the bootstrapping (default = 100).
#' For publications it is recommended that 10,000 runs be performed since sample
#' error goes as 1/sqrt(nRuns).
#'
#'
#' @param data An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
#' where nS is the number of students in the sample and nQ is the number of questions.
#'
#' @return Point-biserial correlation between the total score (with focus item removed)
#' and the focus item. For all items on the assessment
#'
#' @export
#'
#' @examples
#' # Pull in PIQL data from AWS and get some course data.
#' PIQLdata <- pullPIQLdata()
#' temp.piql.data <- PIQLdata$courses
#' data.num <- temp.data$data.num
#'
#' # Point-biserial
#' pointBiserial(data.num)
cttpointBiserial <- function(data, booted = FALSE, nRuns = 100){
  # Get number of student and questions
  nS <- nrow(data)
  nQ <- ncol(data)

  if (booted == FALSE) {
    thing.return <- internal.pointBiserial(data)
    return(thing.return)
  }

  if (booted == TRUE) {
    # Build storage array.
    itemPB.booted <- array(NA, dim = c(nRuns, nQ))
    # Do the runs:
    for (ii in 1:nRuns) {
      # Draw a sample (with replacement) from the full sample
      ## that is the same size as the full sample.
      random.sample <- data[sample(1:nS,nS,replace = TRUE),]
      # Calculate and store disc
      itemPB.booted[ii,] <-  internal.pointBiserial(random.sample)
    }

    # Means for each item across all runs
    item.pb.mn <- apply(itemPB.booted,2,mean)
    # St. Dev. for each item across all runs
    item.pb.sd <- apply(itemPB.booted,2,sd)
    # Format return
    thing.return <- round(t(data.frame(pointBi.Mean = item.pb.mn,
                                       pointBi.StDev = item.pb.sd)), 3)
    if ( is.null(colnames(data.num)) )  {
      colnames(thing.return) <- paste0("Q",c(1:nQ))
    } else {
      colnames(thing.return) <- colnames(data.num)
    }
    return(thing.return)
  }
}
