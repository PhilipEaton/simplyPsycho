#' Compare courses
#'
#' @description Use a t-test to compare total scores between two courses, and
#' ANOVA to compate three or more courses.
#'
#' NOTE: Use the Left and Right arrow in the Plots window to switch between the plots.
#'
#' @param data MUST BE the output of piql.data.select for multiple courses selected.
#'
#' @param nRuns Number of runs to use when calculating error in CTT Difficulty.
#'
#' @param makeBoxPlots (Default = FALSE) If TRUE, then bar plots will be made for
#' all given courses in a single plot.
#'
#' NOTE: Use the Left and Right arrow in the Plots window to switch between the plots.
#'
#' @param makeDiffPlots (Default = FALSE) If TRUE, then point plots will be made for
#' comparing all possible course pairs.
#'
#' NOTE: Use the Left and Right arrow in the Plots window to switch between the plots.
#'
#' @param makeSankeyPlots (Default = FALSE) If TRUE, then Sankey plots will be made for
#' comparing all possible course pairs.
#'
#' NOTE: Use the Left and Right arrow in the Viewer window to switch between the plots.
#'
#' @param nBins Number of total score bins to use for the Sankey plots. Bins are roughly
#' "equal" ranges of total score. ~(number of total scores possible)/nBins
#'
#' @param return.gains (Default = FALSE) If FALSE, the gains are not returned for each student.
#' IF TRUE, then gains are reported for each student.
#'
#' @param makeGainsPlots (Default = FALSE) If true, then plots of normalized gain/change versus
#' "pre" scores are created.
#'
#' @return Cohen's d with interpretation, p-value and adjusted p-value of difference in means,
#' course normalized gain, difference in item difficulty with error. On request
#' box plots, change in difficulty plots, Sankey plots, and gains versus pre score plots
#' cal all be created. Sankey plots and gains versus pre score plots require matched data.
#'
#' NOTE: Use the Left and Right arrow in the Plots window to switch between the plots.
#'
#' @export
#'
#' @examples
#' # Pull sample data
#' temp.data <- piql.data.select(simplySampleData, courses = c(1,2), numBlanks.allowed = 0, Matched = TRUE)
#' gen.compare.courses(temp.data,  makeSankeyPlots = TRUE)
#'
#' gen.compare.courses(data)
#' ## Box Plots can be made
#' gen.compare.courses(temp.data, makeBoxPlots = TRUE)
#' ## Change in item difficulty pots can be made
#' gen.compare.courses(temp.data, makeDiffPlots = TRUE)
#'
#' ### MATCHED DATA
#' ## Sankey plots can be made
#' gen.compare.courses(temp.data, makeSankeyPlots = TRUE)
#' ## Plots of normalized gain/change versus pre score for each student
#' gen.compare.courses(temp.data, makeGainsPlots = TRUE)
#' ## You can get the gain values by setting return.gains = TRUE
#' gen.compare.courses(temp.data, return.gains = TRUE)
#'
gen.compare.courses <- function (data, makeBoxPlots = FALSE, makeDiffPlots = FALSE, makeSankeyPlots = FALSE, nBins = 7, nRuns = 100, return.gains = FALSE, makeGainsPlots = FALSE, ...){
  data.list <- list()
  # Build list of graded data from given courses
  if (typeof(data) == "list") {
    temp.nums <- grep("data.num", names(data))
    temp.names <- substr(names(data)[temp.nums],10,12)
    for (nn in 1:length(temp.nums)) {
      data.list[[nn]] <- data[[ temp.nums[nn] ]]
    }
  }
  # Check that more than one class is given.
  if (typeof(data) == "double" || length(data.list) == 1) {
    stop("You need to supply more than one course for comparision to be done.")
  }
  # Number of questions
  nQ <- ncol(data.list[[1]])
  nS <- nrow(data.list[[1]])
  # Get total scores from files in the data lists
  total.scores <- lapply(data.list, function(x) rowSums(x))
  names(total.scores) <- paste0("Course",temp.names)
  # Get number of courses in data
  nCourses <- length(total.scores)
  # Open storage variables
  results <- as.data.frame(array(NA, dim = c(5,nCourses*(nCourses-1)/2)))
  rownames(results) <- c("Cohen's d", "Interpretation", "p-value", "Adjusted p-value", "Course Norm. Gain")
  diff.results <- as.data.frame(array(NA, dim = c((nCourses*(nCourses-1)), nQ)))
  colnames(diff.results) <- colnames(data.list[[1]])
  item.diff <- cttDifficulty(data, booted = TRUE, nRuns = nRuns)
  norm.gain.full <- as.data.frame(array(NA, dim = c(nS,nCourses)))
  norm.change.full <- as.data.frame(array(NA, dim = c(nS,nCourses)))
  nn <- 1
  for (ii in 1:nCourses) {
    for (jj in 1:nCourses) {
      if (jj > ii) {
        colnames(results)[nn] <- paste0(temp.names[ii], " & ",  temp.names[jj])
        rownames(diff.results)[c(2*nn-1,2*nn)] <- paste0(c("difference.in.dif","error"),  paste0(temp.names[jj], "-",  temp.names[ii]))
        set1 <- total.scores[[ii]]
        n1 <- length(set1)
        mean1 <- mean(set1)
        sd1 <- sd(set1)
        set2 <- total.scores[[jj]]
        n2 <- length(set2)
        mean2 <- mean(set2)
        sd2 <- sd(set2)
        # effect size - Cohen's d and t-test
        s <- sqrt( ((n1-1)*sd1 + (n2-1)*sd2)/(n1+n2-2) )
        cohend.val <- (mean2-mean1) / s
        # Classification
        if (abs(cohend.val) <= 0.01) {cohend.int <- "Very Small"
        } else if (abs(cohend.val) <= 0.20) {cohend.int <- "Small"
        } else if (abs(cohend.val) <= 0.50) {cohend.int <- "Medium"
        } else if (abs(cohend.val) <= 0.80) {cohend.int <- "Large"
        } else if (abs(cohend.val) <= 1.20) {cohend.int <- "Very Large"
        } else if (abs(cohend.val) <= 2.00) {cohend.int <- "Huge"
        }
        # Results
        results[1,nn] <- as.numeric(round(cohend.val,3))
        results[2,nn] <- cohend.int
        results[3,nn] <- t.test(set1,set2)$p.value
        if (nQ != mean1) {results[5,nn] <- (mean2 - mean1)/(nQ-mean1)
        } else {results[5,nn] <- NA}
        # Diff comparisions
        diff.results[2*nn-1,] <- item.diff[[jj]][1,] - item.diff[[ii]][1,]
        diff.results[2*nn,] <- sqrt(item.diff[[jj]][2,]^2 + item.diff[[ii]][2,]^2)
        # Diff Plots
        if (makeDiffPlots == TRUE) {
          error.bar <- function(x, y, upper, lower=upper, length=0.05,...){
            arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
          }
            x <- unlist(diff.results[2*nn-1,])
            x.err <- unlist(diff.results[2*nn,])
            xmax <- max(x)
            if (max(x)<0.25) {xmax <- 0.25} else if (max(x)<0.5) {xmax <- 0.5} else if (max(x)<0.75) {xmax <- 0.75} else {xmax <- 1}
            plot(x, ylab = paste0("Change in Difficulty: ", paste0(temp.names[jj], "-",  temp.names[ii])), ylim = c(-xmax,xmax))
            error.bar(1:nQ, x, x.err)
            abline(h = 0, col = "black")
        }
        if (makeSankeyPlots == TRUE) {
          if (data$Matched == TRUE) {
            general.compare.courses.MakeSankey(set1, set2, nQ, nBins,temp.names, ii, jj)
          } else {print("Sankey plot require MATCHED data.")}
        }
        #Student gains
        if (data$Matched == TRUE) {
          norm.gain <- numeric(nS)
          norm.change <- numeric(nS)
          for (ss in 1:nS) {
            if (nQ != set1[ss]) {
              norm.gain[ss] <- (set2[ss]-set1[ss])/(nQ-set1[ss])
            } else {norm.gain[ss] <- NA}
            if (nQ != set1[ss] && nQ != set2[ss] && 0 != set2[ss] && 0 != set1[ss]) {
              if (set2[ss] > set1[ss]) {
                norm.change[ss] <- (set2[ss]-set1[ss])/(nQ-set1[ss])
              } else {
                norm.change[ss] <- (set2[ss]-set1[ss])/(set1[ss])
              }
            } else {norm.gain[ss] <- NA}
          }
          norm.gain.full[,nn] <- norm.gain
          norm.change.full[,nn] <- norm.change
          colnames(norm.gain.full)[nn] <- paste0(temp.names[jj], "-",  temp.names[ii])
          colnames(norm.change.full)[nn] <- paste0(temp.names[jj], "-",  temp.names[ii])
          if (makeGainsPlots == TRUE) {
            # Gain
            x = set1
            y = norm.gain
            foobar <- array(c(x[1],y[1],1),dim = c(3,1))
            for (hh in 2:length(x)) {
              if (!is.na(y[hh])) {
                if (sum(foobar[2,foobar[1,] == x[hh]] == y[hh]) > 0){
                  temp.x <- which(foobar[1,] == x[hh], arr.ind = TRUE)
                  temp.y <- which(foobar[2,] == y[hh], arr.ind = TRUE)
                  foobar[3,temp.x[!is.na(match(temp.x,temp.y))]] <- foobar[3,temp.x[!is.na(match(temp.x,temp.y))]] + 1
                } else {
                  foobar <- cbind(foobar, c(x[hh],y[hh],1))
                }
              }
            }
            ymax = max(y, na.rm = TRUE) + 0.1
            ymin = min(y, na.rm = TRUE) - 0.1
            plot(foobar[1,], foobar[2,], ylab = paste0("Norm. Gain: ", paste0(temp.names[jj], "-",  temp.names[ii])),
                 xlab = paste0("Score on ", temp.names[ii]), ylim = c(-1,1), pch = 1,
                 cex = round(foobar[3,],0)/4)
            #temp.thing <- data.frame(x = x, y= y)
            #vioplot::vioplot(y~x, data = temp.thing,
            #                 ylab = paste0("Norm. Gain: ", paste0(temp.names[jj], "-",  temp.names[ii])),
            #                 xlab = paste0("Score on ", temp.names[ii]), ylim = c(ymin,ymax))
            abline(h=0)
            # Change
            y = norm.change
            foobar <- array(c(x[1],y[1],1),dim = c(3,1))
            for (hh in 2:length(x)) {
              if (!is.na(y[hh])) {
                if (sum(foobar[2,foobar[1,] == x[hh]] == y[hh]) > 0){
                  temp.x <- which(foobar[1,] == x[hh], arr.ind = TRUE)
                  temp.y <- which(foobar[2,] == y[hh], arr.ind = TRUE)
                  foobar[3,temp.x[!is.na(match(temp.x,temp.y))]] <- foobar[3,temp.x[!is.na(match(temp.x,temp.y))]] + 1
                } else {
                  foobar <- cbind(foobar, c(x[hh],y[hh],1))
                }
              }
            }
            ymax = max(y, na.rm = TRUE) + 0.1
            ymin = min(y, na.rm = TRUE) - 0.1
            plot(foobar[1,], foobar[2,], ylab = paste0("Norm. Change: ", paste0(temp.names[jj], "-",  temp.names[ii])),
                 xlab = paste0("Score on ", temp.names[ii]), ylim = c(-1,1), pch = 1,
                 cex = round(foobar[3,],0)/4)
            abline(h=0)
          }
        }

        # advance nn value
        nn = nn+1
      }
    }
  }
  # Adjust p-values due to multiple comparisions
  results[4,] <- stats::p.adjust(results[3,], ...)
  # Box plots
  if (makeBoxPlots == TRUE) {
    boxplot(total.scores, ylim = c(0,nQ), ylab = "Total Scores")
  }
  # Return results
  thing.return.master <- list()
  thing.return.master$results <- results
  thing.return.master$itemDiff.comparision <- diff.results
  if (data$Matched == TRUE && return.gains == TRUE) {
    thing.return.master$norm.gain <- norm.gain.full
    thing.return.master$norm.change <- norm.change.full
  }
  return(thing.return.master)

}


