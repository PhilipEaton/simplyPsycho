#' Compare courses
#'
#' @description Use a t-test to compare total scores between two courses, and
#' ANOVA to compate three or more courses.
#'
#' @param data MUST BE the output of piql.data.select for multiple courses selected.
#'
#' @param makeBoxPlots (Default = FALSE) If TRUE, then bar plots will be made for
#' all given courses in a single plot.
#'
#' @param makeBoxPlots (Default = FALSE) If TRUE, then point plots will be made for
#' comparing all possible course pairs.
#'
#' NOTE: Use the Left and Right arrow in the Plots window to switch between the plots.
#'
#' @return Gives the Cohen's d, an interpretation of the Cohen's d, the p-value,
#' the adjust p-value for multiple comparisions, and an optional barplot.
#'
#' @export
#'
#' @examples
#' # Pull in PIQL data from AWS and get some course data.
#' PIQLdata <- pullPIQLdata()
#' data <- piql.data.select(PIQLdata, course = c(1,3), Matched = TRUE)
#'
#' # Compare total scores...
#' gen.compare.courses(data, method = "bonferroni")
#' # ... supports makinf boxplots as well
#' gen.compare.courses(data, method = "bonferroni", makeBoxPlots = TRUE)
gen.compare.courses <- function (data, makeBoxPlots = FALSE, makeDiffPlots = FALSE, ...){
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
  # Get total scores
  nQ <- ncol(data.list[[1]])
  total.scores <- lapply(data.list, function(x) rowSums(x))
  names(total.scores) <- paste0("Course",temp.names)
  nCourses <- length(total.scores)
  results <- as.data.frame(array(NA, dim = c(4,nCourses*(nCourses-1)/2)))
  diff.results <- as.data.frame(array(NA, dim = c(2*nCourses, nQ)))
  colnames(diff.results) <- colnames(data.list[[1]])
  rownames(results) <- c("Cohen's d", "Interpretation", "p-value", "Adjusted p-value")
  item.diff <- cttDifficulty(data, booted = TRUE)
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
        cohend.val <- (mean1-mean2) / s
        results[3,nn] <- t.test(set1,set2)$p.value
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
  return(thing.return.master)

}


