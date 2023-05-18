#' CTT Cronbach's Alpha (of multiple courses) with bootstrapping for error estimation:
#'
#' @description Calculate Cronbach's Alpha for an assessment from sample data.
#'
#' @param data Can be:
#'
#' 1) An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
#' where nS is the number of students in the sample and nQ is the number of questions.
#'
#' 2) The output of piql.data.select() for one or multiple courses.
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
#' PIQLdata <- pullPIQLdata()
#' temp.data <- piql.data.select(PIQLdata, course = 2, numBlanks.allowed = 0)
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
  data.list <- list()

  if (typeof(data) == "list") {
    temp.nums <- grep("data.num", names(data))
    temp.names <- substr(names(data)[temp.nums],10,12)
    for (nn in 1:length(temp.nums)) {
      data.list[[nn]] <- data[[ temp.nums[nn] ]]
    }
  }

  if (typeof(data) == "double") {
    temp.nums <- 1
    temp.names <- ""
    data.list[[1]] <- data
  }

  thing.return.master <- list()
  for (nn in 1:length(temp.nums)) {
    data <- data.list[[nn]]
    # Get number of student and questions
    nS <- nrow(data)
    nQ <- ncol(data)

    if (booted == FALSE) {
      thing.return <- round(t(data.frame(cronbachAlpha(data))), 3)
      colnames(thing.return) <- "Results"
      thing.return.master[[nn]] <- thing.return
      names(thing.return.master)[nn] <- noquote(paste0("cttCronbachAlpha", temp.names[nn]))
    }

    if (booted == TRUE) {
      # Build storage array.
      cronAlpha.booted <- array(NA, dim = c(nRuns, 1))
      # Do the runs:
      for (ii in 1:nRuns) {
        # Draw a sample (with replacement) from the full sample
        ## that is the same size as the full sample.
        random.sample <- data[sample(1:nS,nS,replace = TRUE),]
        # Calculate and store disc
        cronAlpha.booted[ii] <-  cronbachAlpha(random.sample)
      }

      # Means for each item across all runs
      item.mn <- apply(cronAlpha.booted,2,mean)
      # St. Dev. for each item across all runs
      item.sd <- apply(cronAlpha.booted,2,sd)
      # Format return
      thing.return <- round(t(data.frame(cttCronbackAlpha.Mean = item.mn,
                                         cttCronbackAlpha.StDev = item.sd)), 3)
      colnames(thing.return) <- "Results"
      thing.return.master[[nn]] <- thing.return
      names(thing.return.master)[nn] <- noquote(paste0("cttCronbackAlpha", temp.names[nn]))
    }
  }
  return(thing.return.master)
}
