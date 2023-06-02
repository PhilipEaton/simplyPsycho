#' CTT Discrimination of multiple courses with bootstrapping for error estimation and plotting:
#'
#' @description Calculate Classical Test Theory item discrimination for the items on
#' an assessment using a given sample.
#'
#' @param data Can be either
#'
#' 1) An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
#' where nS is the number of students in the sample and nQ is the number of questions,
#'
#' 2) The output of piql.data.select with one or multiple courses selected.
#'
#' @param perc Top and bottom percentage to be compared (default = 0.27).
#'
#' @param as.Percentile Calculate item discrimination user percentiles as opposed to
#' raw percentages.
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
#' standard deviation for the CTT item discrimination calculated from nRuns randomly sampled
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
#' # Straight item discrimination
#' cttDiscrimination(data.num)
#'
#' # Booted using 100 random samples (default)
#' cttDiscrimination(data.num, booted = TRUE)
#'
#' # Booted using 1000 random samples by manually setting nRuns
#' cttDiscrimination(data.num, booted = TRUE, nRuns = 1000)
cttDiscrimination <- function (data, perc = 0.27, as.Percentile = FALSE,  booted = FALSE, nRuns = 100, plotBarChart = FALSE){
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
  thing.return.master.plot <- list()
  thing.return <- list()
  for (nn in 1:length(temp.nums)) {
    data <- data.list[[nn]]
    # Get number of student and questions
    nS <- nrow(data)
    nQ <- ncol(data)
    total.scores <- rowSums(data)
    if (booted == FALSE) {
      # ctt Discrimination
      if (as.Percentile == FALSE) {
        thing.return <- t(data.frame(cttDisc = round(colMeans(data[order(total.scores, decreasing = TRUE),][c(1:round(nS*perc,0)),]) -
                                                       colMeans(data[order(total.scores, decreasing = FALSE),][c(1:round(nS*perc,0)),]), 3)))
        colnames(thing.return) <- paste0("Q", 1:nQ)
        thing.return.master[[nn]] <- thing.return
        names(thing.return.master)[nn] <- noquote(paste0("cttDisc", temp.names[nn]))
        thing.return.master.plot[[nn]] <- thing.return
      } else {
        top.score <- total.scores[order(total.scores, decreasing = TRUE)][round(nS*perc,0)]
        bot.score <- total.scores[order(total.scores, decreasing = FALSE)][round(nS*perc,0)]
        top.students <- data[total.scores >= top.score,]
        bot.students <- data[total.scores <= bot.score,]
        ntop <- nrow(top.students)
        nbot <- nrow(bot.students)
        disc <- t(as.data.frame(round(colMeans(top.students) - colMeans(bot.students),3)))
        thing.return <- data.frame(disc, ntop, nbot)
        row.names(thing.return) <- ""
        colnames(thing.return) <- c(paste0("Q",c(1:nQ)), "nS.top", "nS.bot")

        thing.return.master[[nn]] <- thing.return
        names(thing.return.master)[nn] <- noquote(paste0("cttDisc", temp.names[nn]))
        thing.return.master.plot[[nn]] <- thing.return[1:nQ]
      }
    }

    if (booted == TRUE) {
      # Build storage array.
      if (as.Percentile == FALSE) {
        itemDisc.booted <- array(NA, dim = c(nRuns, nQ))
        colnames(itemDisc.booted) <- paste0("Q",c(1:nQ))
      } else {
        itemDisc.booted <- array(NA, dim = c(nRuns, nQ+2))
        colnames(itemDisc.booted) <- c(paste0("Q",c(1:nQ)),"nS.top", "nS.bot")
        }

      # Do the runs:
      for (ii in 1:nRuns) {
        # Draw a sample (with replacement) from the full sample
        ## that is the same size as the full sample.
        random.sample <- data[sample(1:nS,nS,replace = TRUE),]
        total.scores <- rowSums(random.sample)
        # Calculate and store disc
        if (as.Percentile == FALSE) {
          itemDisc.booted[ii,] <-  t(data.frame(round(colMeans(random.sample[order(total.scores, decreasing = TRUE),][c(1:round(nS*perc,0)),]) -
                                                        colMeans(random.sample[order(total.scores, decreasing = FALSE),][c(1:round(nS*perc,0)),]), 3)))
        } else {
          top.score <- total.scores[order(total.scores, decreasing = TRUE)][round(nS*perc,0)]
          bot.score <- total.scores[order(total.scores, decreasing = FALSE)][round(nS*perc,0)]
          top.students <- random.sample[total.scores >= top.score,]
          bot.students <- random.sample[total.scores <= bot.score,]
          ntop <- nrow(top.students)
          nbot <- nrow(bot.students)
          disc <- as.vector(round(colMeans(top.students) - colMeans(bot.students),3))
          itemDisc.booted[ii,] <-  t(as.data.frame(c(disc,ntop,nbot)))
        }
      }

      # Means for each item across all runs
      item.disc.mn <- apply(itemDisc.booted,2,mean)
      # St. Dev. for each item across all runs
      item.disc.sd <- apply(itemDisc.booted,2,sd)
      # Format return
      thing.return <- round(t(data.frame(Mean = item.disc.mn,
                                         StDev = item.disc.sd)), 3)
      if (as.Percentile == FALSE) {
        thing.return.master[[nn]] <- thing.return
        names(thing.return.master)[nn] <- noquote(paste0("cttDisc", temp.names[nn]))
        thing.return.master.plot[[nn]] <- thing.return
      } else {
        thing.return.master[[nn]] <- thing.return
        names(thing.return.master)[nn] <- noquote(paste0("cttDisc", temp.names[nn]))
        thing.return.master.plot[[nn]] <- thing.return[,1:nQ]
      }
    }
  }

  error.bar <- function(x, y, upper, lower=upper, length=0.05,...){
    arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
  }

  if (plotBarChart == TRUE) {
    if (length(thing.return.master.plot) == 1) {
      temp <- barplot(unlist(thing.return.master.plot[[1]][1,1:nQ]), col = c("lightblue"),
                      ylim = c(0,1), xaxt='n')
      axis(1, at = temp, labels = names(thing.return.master[[1]][1,1:nQ]), cex.axis = 0.75, padj = -1)
      title(xlab = "Questions", line = 2, cex.lab=1.2)
      title(ylab = "CTT Discrimination", line = 2, cex.lab=1.2)
      abline(h = c(0.3), col = "red")
      if (booted == TRUE) {
        error.bar(temp, unlist(thing.return.master.plot[[1]][1,]), unlist(thing.return.master.plot[[1]][2,]))
      }
    }
    if (length(thing.return.master.plot) > 1) {
      plot.thing <- unlist(thing.return.master.plot[[1]][1,1:nQ])
      if (booted == TRUE) {
        plot.thing.eb <- unlist(thing.return.master.plot[[1]][2,1:nQ])
      }
      for (mm in 2:length(thing.return.master.plot)) {
        plot.thing <- rbind(plot.thing, unlist(thing.return.master.plot[[mm]][1,]))
        if (booted == TRUE) {
          plot.thing.eb <- rbind(plot.thing.eb, unlist(thing.return.master.plot[[mm]][2,]))
        }
      }
      temp <- barplot(plot.thing, col = c(1:length(thing.return.master.plot)) + 1, beside = TRUE,
                      ylim = c(0,1), xaxt='n')
      temp.label.locations <- temp[round(nrow(temp)/2,0),]
      axis(1, at = temp.label.locations, labels = names(thing.return.master[[1]][1,1:nQ]), cex.axis = 0.75, padj = -1)
      title(xlab = "Questions", line = 2, cex.lab=1.2)
      title(ylab = "CTT Discrimination", line = 2, cex.lab=1.2)
      abline(h = c(0.3), col = "red")
      legend("topright", legend=c(temp.names), col= c(1:length(thing.return.master)) + 1,
             lty = 1, cex=1, box.lty=0, ncol = length(temp.names))
      if (booted == TRUE) {
        error.bar(temp, as.matrix(plot.thing), as.matrix(plot.thing.eb))
      }
    }
  }

  return(thing.return.master)
}

