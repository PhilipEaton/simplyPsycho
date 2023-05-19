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
gen.compare.courses <- function (data, makeBoxPlots = FALSE, ...){
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
  nCorses <- length(total.scores)
  results <- as.data.frame(array(NA, dim = c(4,nCorses*(nCorses-1)/2)))
  rownames(results) <- c("Cohen's d", "Interpretation", "p-value", "Adjusted p-value")
  nn <- 1
  for (ii in 1:nCorses) {
    for (jj in 1:nCorses) {
      if (jj > ii) {
        colnames(results)[nn] <- paste0(temp.names[ii], " & ",  temp.names[jj])
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
  return(results)

}


