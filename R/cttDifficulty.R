#' CTT Difficulty of multiple courses with bootstrapping for error estimation and plotting:
#'
#' @description Calculate Classical Test Theory item difficulty for the items on
#' an assessment using a given sample.
#'
#' @param data Can be either
#'
#' 1) An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
#' where nS is the number of students in the sample and nQ is the number of questions,
#'
#' 2) The output of piql.data.select with one or multiple courses selected.
#'
#' @param booted Logical (default = FALSE). FALSE means no bootstrapping will be
#' performed. TRUE turns on the bootstrapping feature.
#'
#' @param nRuns Number of random samples to use in the bootstrapping (default = 100).
#' For publications it is recommended that 10,000 runs be performed since sample
#' error goes as 1/sqrt(nRuns).
#'
#' @param plotBarChart (Default = FALSE). Generate barchart. If bootstrapping was
#' performed, then error bars wll be automatically added (+- 1 standard deviation).
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
#' PIQLdata <- pullPIQLdata()
#' temp.data <- piql.data.select(PIQLdata, course = 2, numBlanks.allowed = 0)
#' data.num <- temp.data$data.num
#'
#' # Straight item difficulty.
#' cttDifficulty(data.num)
#'
#' # Booted using 100 random samples (default)
#' cttDifficulty(data.num, booted = TRUE)
#'
#' # Booted using 1000 random samples by manually setting nRuns
#' cttDifficulty(data.num, booted = TRUE, nRuns = 1000)
cttDifficulty <- function (data, booted = FALSE, nRuns = 100, plotBarChart = FALSE){
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
      # ctt Difficulty is just the mean sccore with graded as 0/1.
      thing.return <- round(t(data.frame(cttDiff = colMeans(data))), 3)
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
      if ( is.null(colnames(data)) )  {
        colnames(thing.return) <- paste0("Q",c(1:nQ))
      } else {
        colnames(thing.return) <- colnames(data)
      }
      thing.return.master[[nn]] <- thing.return
      names(thing.return.master)[nn] <- noquote(paste0("cttDiff", temp.names[nn]))
    }
  }

  error.bar <- function(x, y, upper, lower=upper, length=0.05,...){
    arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
  }

  if (plotBarChart == TRUE) {
    if (length(thing.return.master) == 1) {
      temp <- barplot(thing.return.master[[1]][1,], col = c("lightblue"),
                      ylim = c(0,1), xaxt='n')
      axis(1, at = temp, labels = names(thing.return.master[[1]][1,]), cex.axis = 0.75, padj = -1)
      title(xlab = "Questions", line = 2, cex.lab=1.2)
      title(ylab = "CTT Difficulty", line = 2, cex.lab=1.2)
      abline(h = c(0.2,0.8), col = "red")
      if (booted == TRUE) {
        error.bar(temp, thing.return.master[[1]][1,], thing.return.master[[1]][2,])
      }
    }
    if (length(thing.return.master) > 1) {
      plot.thing <- thing.return.master[[1]][1,]
      if (booted == TRUE) {
        plot.thing.eb <- thing.return.master[[1]][2,]
      }
      for (mm in 2:length(thing.return.master)) {
        plot.thing <- rbind(plot.thing, thing.return.master[[mm]][1,])
        if (booted == TRUE) {
          plot.thing.eb <- rbind(plot.thing.eb, thing.return.master[[mm]][2,])
        }
      }
      temp <- barplot(plot.thing, beside = TRUE,
                      ylim = c(0,1), xaxt='n' , col = safe_colorblind_palette[c(1:length(thing.return.master))])
      temp.label.locations <- temp[round(nrow(temp)/2,0),]
      axis(1, at = temp.label.locations, labels = names(thing.return.master[[1]][1,]), cex.axis = 0.75, padj = -1)
      title(xlab = "Questions", line = 2, cex.lab=1.2)
      title(ylab = "CTT Difficulty", line = 2, cex.lab=1.2)
      abline(h = c(0.2,0.8), col = "red")
      legend("topright", legend=c(temp.names), col= safe_colorblind_palette[c(1:length(thing.return.master))],
              lty = 1, cex=1, box.lty=0, ncol = length(temp.names), lwd = 2)
      if (booted == TRUE) {
        error.bar(temp, as.matrix(plot.thing), as.matrix(plot.thing.eb))
      }
    }
  }

  return(thing.return.master)
}

