#' MCMR response frequency and correlations
#'
#' @description Calculates the response frequency and correlations for the MCMR
#' items on the assessment.
#'
#' @param data Must be from the piql.data.select or data.select.Walkthrough functions
#'
#' @param booted Logical (default = FALSE). FALSE means no bootstrapping will be
#' performed. TRUE turns on the bootstrapping feature.
#'
#' @param nRuns Number of random samples to use in the bootstrapping (default = 5).
#' For publications it is recommended that 10,000 runs be performed since sample
#' error goes as 1/sqrt(nRuns).
#'
#' @param makeCorPlot (Default = FALSE) If TRUE, then visualizations of the correlation
#' matrices will be produced. When booted = true, the mean correlations will be
#' plotted for each course.
#'
#' @return The response frequency and correlations for the MCMR
#' items on the assessment.
#'
#' @export
#'
#' @examples
#' # Pull in PIQL data from AWS and get some course data.
#' temp.data <- piql.data.select(simplySampleData, courses = 1, numBlanks.allowed = 0)
#'
#' # Get selection numbers and correlations
#' MCMR.response.analysis(temp.data)
#'
#' # Can be booted
#' MCMR.response.analysis(temp.data, booted = TRUE)
#'
#' # Can make plots of the correlations matrices
#' MCMR.response.analysis(temp.data, makeCorPlot = TRUE)
MCMR.response.analysis <- function (data, booted = FALSE, nRuns = 5, makeCorPlot = FALSE){
  data.list <- list()
  MCMR.items <- data$MCMRitems

  if (typeof(data) == "list") {
    temp.nums <- grep("data.alpha", names(data))
    temp.names <-  unlist(as.data.frame(strsplit(names(data)[temp.nums],"\\."))[3,])
    for (nn in 1:length(temp.nums)) {
      data.list[[nn]] <- data[[ temp.nums[nn] ]]
    }
  } else {stop("data should be the result of either data.select function. Check your data input.")}

  thing.return.master <- list()

  for (nn in 1:length(temp.nums)) {
    data <- data.list[[nn]]
    data <- data[,MCMR.items]
    # Get number of student and questions
    nS <- nrow(data)
    nQ <- ncol(data)
    # Separate MCMR items
    ## First get options for each question
    MCMR.selections <- list()
    option.list <- list()
    for (qq in 1:nQ) {
      temp.sep <- unlist(strsplit(data[,qq], split = ""))
      option.list[[qq]] <- sort(temp.sep[!duplicated(temp.sep)])
      MCMR.selections[[qq]] <- array(0, dim = c(nS, length(option.list[[qq]])))
      colnames(MCMR.selections[[qq]]) <- paste("Q", MCMR.items[qq], option.list[[qq]], sep = "")
    }
    MCMR.selections.initial <- MCMR.selections

    if (booted == FALSE) {
      # Count option selections
      MCMR.selections <- MCMR.selections.initial
      for (ss in 1:nS) {
        for (qq in 1:nQ) {
          MCMR.selections[[qq]][ss,match(as.vector(unlist(strsplit(data[ss,qq], split = ""))), option.list[[qq]])] <- 1
        }
      }
      MCMR.selections.results <- lapply(MCMR.selections, function(x) colSums(x))
      names(MCMR.selections.results) <- paste0("Q", MCMR.items, ".Resp.Freq")
      # Combine to get correlation
      MCMR.selections.final <- MCMR.selections[[1]]
      for (qq in 2:nQ) {
        MCMR.selections.final <- cbind(MCMR.selections.final, MCMR.selections[[qq]])
        colSums(MCMR.selections.final)
      }
      # Format return
      thing.return <-  list()
      thing.return$selections <- MCMR.selections.results
      thing.return$correlations <- round(cor(MCMR.selections.final),3)
      thing.return.master[[nn]] <- thing.return
      names(thing.return.master)[nn] <- noquote(paste0("MCMR.Option.Results", temp.names[nn]))
      # Plot if TRUE
      if (makeCorPlot == TRUE) {
        corrplot::corrplot(thing.return$correlations, tl.cex = 0.75,
                           title = paste0("MCMR Option Correlations for ", temp.names[nn]),
                           mar = c(1,1,1.1,1))
      }
    }

    if (booted == TRUE) {
      # Build storage array.
      MCMR.selections.booted <- array(0, dim = c(nRuns, nS, length(unlist(option.list))))
      # Do the runs:
      for (ii in 1:nRuns) {
        # Draw a sample (with replacement) from the full sample
        ## that is the same size as the full sample.
        random.sample <- data[sample(1:nS,nS,replace = TRUE),]
        # Count option selections
        MCMR.selections <- MCMR.selections.initial
        for (ss in 1:nS) {
          for (qq in 1:nQ) {
            MCMR.selections[[qq]][ss,match(as.vector(unlist(strsplit(random.sample[ss,qq], split = ""))), option.list[[qq]])] <- 1
          }
        }
        MCMR.selections.final <- MCMR.selections[[1]]
        for (qq in 2:nQ) {
          MCMR.selections.final <- cbind(MCMR.selections.final, MCMR.selections[[qq]])
        }
        MCMR.selections.booted[ii,,] <- MCMR.selections.final
      }
      # Get total selections per runs
      MCMR.selections.mn <- round(rowMeans(apply(MCMR.selections.booted, 1, colSums)), 3)
      names(MCMR.selections.mn) <- colnames(MCMR.selections.final)
      MCMR.selections.sd <- round(apply(apply(MCMR.selections.booted, 1, colSums), 1, sd), 3)
      names(MCMR.selections.sd) <- colnames(MCMR.selections.final)
      # Correlations
      MCMR.correlations.booted <- array(0, dim = c(nRuns, length(unlist(option.list)), length(unlist(option.list))))
      for (rr in 1:nRuns) {
        retained <- colSums(MCMR.selections.booted[rr,,]) != 0
        MCMR.correlations.booted[rr,retained,retained] <- cor(MCMR.selections.booted[rr,retained,retained])
      }
      MCMR.correlations.mn <- round(apply(MCMR.correlations.booted, c(2,3), mean),3)
      colnames(MCMR.correlations.mn) <- colnames(MCMR.selections.final)
      rownames(MCMR.correlations.mn) <- colnames(MCMR.selections.final)
      MCMR.correlations.sd <- round(apply(MCMR.correlations.booted, c(2,3), sd),3)
      colnames(MCMR.correlations.sd) <- colnames(MCMR.selections.final)
      rownames(MCMR.correlations.sd) <- colnames(MCMR.selections.final)

      # Format return
      thing.return <-  list()
      thing.return$selectionMeans <- MCMR.selections.mn
      thing.return$selectionStDevs <- MCMR.selections.sd
      thing.return$correlationMeans <- MCMR.correlations.mn
      thing.return$correlationStDevs <- MCMR.correlations.sd
      thing.return.master[[nn]] <- thing.return
      names(thing.return.master)[nn] <- noquote(paste0("MCMR.Option.Results", temp.names[nn]))

      # Plot if TRUE
      if (makeCorPlot == TRUE) {
        corrplot::corrplot(thing.return$correlationMeans, tl.cex = 0.75,
                           title = paste0("MCMR Option Correlations for ", temp.names[nn]),
                           mar = c(1,1,1.1,1))
      }
    }
  }



  return(thing.return.master)
}

