#' Apply multple imputation to given data sets
#'
#' @description Uses multiple implutation to create nImps complete data sets.
#'
#' @param data Can be either
#'
#' 1) An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
#' where nS is the number of students in the sample and nQ is the number of questions,
#'
#' 2) The output of piql.data.select with one or multiple courses selected.
#'
#' @param answerKey Answer key for the assessment being analyzted. Generally can
#' be entered as something like pulled.data$answerKey from the pulldata function.
#'
#' @param remove.lessthan.50perc.answered (Default = TRUE) Removed students who
#' answered less than 50 percent of the questions on the assessment. Removal
#' of these students is standard prectice in multiple imputation practice. If
#' you want to keep all students, set remove.lessthan.50perc.answered = TRUE.
#'
#' @param plot.missing.pattern (Default = FALSE) If TRUE, then a plot of the
#' missing data patterns will be produced.
#'
#' @param nImps (Dfault = 5) Number of imputed data sets to be created.
#'
#' @param nMaxIterations (Default = 5) Number of iterations to be used to create
#' mean values used to estimate missing data. Larger numbers take longer to run,
#' but will give for consistent results.
#'
#' @return List of complete, imputed data sets for the given data formatted to be
#' readily used in all simplyPsycho functions. Suggestion, do not make plots of
#' this data using other functions. Get the results and then use the pool.imputations
#' function to get the pooled results for the imputed courses.
#'
#' @export
#'
#' @examples
#' # Pull in PIQL data from AWS and get some course data.
#' PIQLdata <- pullPIQLdata()
#' temp.data <- piql.data.select(PIQLdata, course = 2, numBlanks.allowed = 0)
#' answerKey <- PIQLdata$answerkey
#'
#' multiple.imputations(temp.data, answerKey)

# Pull in data and the answer key (Thtese will be the dependent variables)
# Variables = temp.data and answerKey
multiple.imputations <- function(data, answerKey, remove.lessthan.50perc.answered = TRUE, plot.missing.pattern = FALSE, nImps = 5, nMaxIterations = 5) {
  data.list <- list()

  if (typeof(data) == "list") {
    temp.nums <- grep("data.alpha", names(data))
    temp.names <- unlist(strsplit(names(data)[temp.nums], split = "\\."))[3*c(1:length(temp.nums))]
    for (nn in 1:length(temp.nums)) {
      data.list[[nn]] <- data[[ temp.nums[nn] ]]
    }
  }

  if (typeof(data) == "double") {
    temp.nums <- 1
    temp.names <- ""
    data.list[[1]] <- data
  }

  if (typeof(answerKey) == "list") {
    answerKey <- unlist(answerKey)
  }


  thing.return.master <- list()
  temp.return <- list()
  for (nn in 1:length(temp.nums)) {
      # ------------------------------------------#
      # Gather needed information from temp.data
      # ------------------------------------------#
      # Get the alphabetical data from temp.data
      data.alpha <- data.list[[nn]]
      # Get number of students and questions and the location of any MCMR items.
      nS <- nrow(data.alpha)
      nQ <- ncol(data.alpha)

      # ------------------------------------------#
      # Separate SR and MCMR items
      # ------------------------------------------#
      if (length(temp.data$MCMRitems) > 0) {
        MCMRitems <- temp.data$MCMRitems
        # Get SR items and code letters into numbers
        data.SR <- as.data.frame(lapply(data.alpha[-MCMRitems], function(x) car::recode(x,
                                      "'A' = 1; 'B' = 2; 'C' = 3; 'D' = 4; 'E' = 5;
                                      'F' = 6; 'G' = 7; 'H' = 8; 'I' = 9; 'J' = 10")))
        # Get MCMR items and grade using the 4 point scale.
        data.MCMR <- data.alpha[MCMRitems]

        # Get the 4-scle scores for the indicated MCMR items.
        key <- car::recode(answerKey[-MCMRitems], "'A' = 1; 'B' = 2;'C' = 3;'D' = 4;'E' = 5;'F' = 6;
                 'G' = 7;'H' = 8;'I' = 9;'J' = 10")
        data.MCMR.score <- array(0, dim = c(nS, length(MCMRitems)))
        colnames(data.MCMR.score) <- colnames(data.MCMR)
        for (ss in 1:nS) {
            for (qq in 1:length(MCMRitems) ) {
              if(is.na(data.MCMR[ss,qq])) {
                data.MCMR.score[ss,qq] = NA
              } else {
                num.True <- sum(unlist(strsplit(data.MCMR[ss,qq], split = "")) %in% unlist(strsplit(answerKey[MCMRitems[qq]], split = "")))
                num.False <- length(unlist(strsplit(data.MCMR[ss,qq], split = ""))) - num.True
                num.cor <- length(unlist(strsplit(answerKey[MCMRitems[qq]], split = "")))
                if (num.True == 0) {
                  data.MCMR.score[ss,qq] = 1
                } else if (num.False > 0) {
                  data.MCMR.score[ss,qq] = 2
                } else if (num.True != num.cor) {
                  data.MCMR.score[ss,qq] = 3
                } else if (num.True == num.cor) {
                  data.MCMR.score[ss,qq] = 4
                }
              }
            }
          }
        # Recombine SR and MCMR items.
        data.full <- cbind(data.SR, data.MCMR.score)
      } else {
        # Get SR items and code letters into numbers
        data.full <- as.data.frame(lapply(data.alpha, function(x) car::recode(x,
                                      "'A' = 1; 'B' = 2; 'C' = 3; 'D' = 4; 'E' = 5;
                                      'F' = 6; 'G' = 7; 'H' = 8; 'I' = 9; 'J' = 10")))
      }

      # ------------------------------------------#
      # Clean data, if needed
      # ------------------------------------------#
      # Remove students who answered less than 50 percent of the questions on the
      # assessment. This is common practice in the multiple imputation literature.
      if (remove.lessthan.50perc.answered == TRUE) {
        pMiss <- function(x) {sum(is.na(x))/length(x)*100}
        data.full <- data.full[apply(data.full,1,pMiss) < 50,]
        nS <- nrow(data.full)
      }
      number.missing.per.question <- colSums(is.na(data.full))


      # ------------------------------------------#
      # Check patterns of missing data
      # ------------------------------------------#
      if (plot.missing.pattern == TRUE) {
        mice::md.pattern(data.full, rotate.names = TRUE)
      }

      # ------------------------------------------#
      # Apply Multiple imputation
      # ------------------------------------------#
      tempData <- mice::mice(data.full,m = nImps, maxit = nMaxIterations, meth='pmm')

      # ------------------------------------------#
      # Return completed sets
      # ------------------------------------------#
      for (hh in 1:nImps) {
        temp.imp.data <- mice::complete(tempData, hh)
        # Recode back to letters
        if (length(temp.data$MCMRitems) > 0) {
        temp.imp.data[,-MCMRitems] <- noquote(apply(temp.imp.data[,-MCMRitems], c(1,2), function(x) LETTERS[x]))
        completedData.SR <- temp.imp.data[,-MCMRitems]
        completedData.MCMR <- temp.imp.data[,MCMRitems]
        # Grade SR items
        completedData.SR <- apply((t((t(completedData.SR) == answerKey[-MCMRitems]))), c(1,2), as.integer)
        # Grade MCMR from 4scale scores (4 = 1, else = 0)
        completedData.MCMR[completedData.MCMR != 4] = 0
        completedData.MCMR[completedData.MCMR == 4] = 1
        scores.imputed <- cbind(completedData.SR,completedData.MCMR)
        } else {
          temp.imp.data <- noquote(apply(temp.imp.data, c(1,2), function(x) LETTERS[x]))
          completedData.SR <- temp.imp.data
          # Grade items
          scores.imputed <- apply((t((t(completedData.SR) == answerKey))), c(1,2), as.integer)
        }
        temp.return[[(2*nImps)*(nn-1) + (2*hh - 1)]] <- temp.imp.data
        temp.return[[(2*nImps)*(nn-1) + (2*hh)]] <- scores.imputed
        names(temp.return)[c(((2*nImps)*(nn-1) + (2*hh - 1)) : ((2*nImps)*(nn-1) + (2*hh)))] <- c(paste0("data.alpha.", temp.names[nn], ".Imp", hh),
                                                paste0("data.num.", temp.names[nn], ".Imp", hh))
      }
  }

  # ------------------------------------------#
  # return
  # ------------------------------------------#
  temp.return$MCMRitems <- data$MCMRitems
  temp.return$Matched <- data$Matched
  return(temp.return)
}
