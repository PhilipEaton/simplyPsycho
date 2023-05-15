#' CTT Difficulty with bootstrapping for error estimation:
#'
#' @description Calculate Classical Test Theory item difficulty for the items on
#' an assessment using a given sample.
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
#' standard deviation for the CTT item difficulties calculated from nRuns randomly sampled
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
#' cttDiff(data.num)
#'
#' # Booted using 100 random samples (default)
#' cttDiff(data.num, booted = TRUE)
#'
#' # Booted using 1000 random samples by manually setting nRuns
#' cttDiff(data.num, booted = TRUE, nRuns = 1000)
cttDiff.mult <- function (data, booted = FALSE, nRuns = 100, plotBarChart = FALSE){
  data.list <- list()

  if (typeof(data) == "list") {
    temp.nums <- grep("data.num.", names(data))
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
      # ctt Difficulty is just the mean sccore with graded as 0/1.
      thing.return <- round(t(data.frame(cttDiff = colMeans(data))), 3)
      if ( is.null(colnames(data.num)) )  {
        colnames(thing.return) <- paste0("Q",c(1:nQ))
      } else {
        colnames(thing.return) <- colnames(data.num)
      }
      thing.return.master[[nn]] <- thing.return
      names(thing.return.master)[nn] <- noquote(paste0("cttDiff", temp.names[nn]))
    }

    if (booted == TRUE) {
      # Build storage array.
      itemDiff.booted <- array(NA, dim = c(nRuns, nQ))
      # Do the runs:
      for (ii in 1:nRuns) {
        # Draw a sample (with replacement) from the full sample
        ## that is the same size as the full sample.
        random.sample <- data[sample(1:nS,nS,replace = TRUE),]
        # Calculate and store disc
        itemDiff.booted[ii,] <-  colMeans(random.sample)
      }

      # Means for each item across all runs
      item.diff.mn <- apply(itemDiff.booted,2,mean)
      # St. Dev. for each item across all runs
      item.diff.sd <- apply(itemDiff.booted,2,sd)
      # Format return
      thing.return <- round(t(data.frame(cttDiff.Mean = item.diff.mn,
                                         cttDiff.StDev = item.diff.sd)), 3)
      if ( is.null(colnames(data.num)) )  {
        colnames(thing.return) <- paste0("Q",c(1:nQ))
      } else {
        colnames(thing.return) <- colnames(data.num)
      }
      thing.return.master[[nn]] <- thing.return
      names(thing.return.master)[nn] <- noquote(paste0("cttDiff", temp.names[nn]))
    }
  }

  if (plotBarChart == TRUE) {
    if (length(thing.return.master) == 1) {
      barplot(thing.return.master[[1]][1,], col = c("darkblue"),
              ylim = c(0,1))
    }
    if (length(thing.return.master) > 1) {
      plot.thing <- thing.return.master[[1]][1,]
      for (mm in 2:length(thing.return.master)) {
        plot.thing <- rbind(plot.thing, thing.return.master[[mm]][1,])
      }
      #c("darkblue","red")
      barplot(plot.thing, col = 1:length(thing.return.master), beside = TRUE,
              ylim = c(0,1))
    }
  }

  return(thing.return.master)
}

