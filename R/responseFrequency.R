#' CTT Difficulty of multiple courses with bootstrapping for error estimation and plotting:
#'
#' @description Calculate Classical Test Theory item difficulty for the items on
#' an assessment using a given sample.
#'
#' @param data Can be either
#'
#' 1) An nS by nQ matrix or data frame of ALPHABETICAL data,
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
#' @param MCMR.separate (Default = TRUE). MCMR.separate = TRUE separates multiple selections
#' to get percentage for individual options on the MCMR items. MCMR.separate = FALSE
#' treats grouped selections as individual options.
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
#' data <- piql.data.select(PIQLdata, course = c(1,2), numBlanks.allowed = 0)
#'
#' # Get Response frequencies for each item in each selected course
#' response.Frequency(data)
#' # Supports bootstrapping
#' response.Frequency(data, booted = TRUE)
#'
response.Frequency <- function (data, booted = FALSE, nRuns = 10, MCMR.separate = TRUE) {
  data.list <- list()

  if (typeof(data) == "list" && typeof(data[[1]]) == "list") {
    temp.nums <- grep("data.alpha", names(data))
    temp.names <- substr(names(data)[temp.nums],12,14)
    for (nn in 1:length(temp.nums)) {
      data.list[[nn]] <- data[[ temp.nums[nn] ]]
    }
  }

  if (typeof(data) == "list" && typeof(data[[1]]) == "character") {
    temp.nums <- 1
    temp.names <- ""
    data.list[[1]] <- data
  }

  thing.return.master <- list()
  for (nn in 1:length(temp.nums)) {
    data.alpha <- data.list[[nn]]
    nQ <- ncol(data.alpha)
    nS <- nrow(data.alpha)
    if (MCMR.separate == TRUE) {
      option.names <- names(table(unlist(lapply(as.list( data.alpha ), function(x) strsplit(x, split = "")))))
    } else if (MCMR.separate == FALSE) {option.names <- names(table(unlist(as.list(data.alpha))))}
    if (booted == FALSE) {
      nO <- length(option.names)
      thing.return <- array(NA, dim = c(nQ,nO))
      colnames(thing.return) <- option.names
      rownames(thing.return) <- colnames(data.alpha)
      for (qq in 1:nQ) {
        cur.table <- table(unlist(lapply(as.list( data.alpha[,qq] ), function(x) strsplit(x, split = ""))))
        cur.table.names <- names(cur.table)
        cur.table.perc <- cur.table/sum(!is.na(data.alpha[,qq]))
        for (rr in 1:length(cur.table.names)) {
          temp.loc <- match(cur.table.names[rr],option.names)
          thing.return[qq,temp.loc] = cur.table.perc[rr]
        }
      }
      thing.return.master[[nn]] <- round(thing.return,3)
      names(thing.return.master)[nn] <- noquote(paste0("respFreq.", temp.names[nn]))
    }

    if (booted == TRUE) {
      nO <- length(option.names)
      res.freq.booted <- array(NA, dim = c(nRuns, nQ, nO))
      thing.return <- array(0, dim = c(nQ,nO))
      for (ii in 1:nRuns) {
        # Draw a sample (with replacement) from the full sample
        ## that is the same size as the full sample.
        random.sample <- data.alpha[sample(1:nS,nS,replace = TRUE),]
        colnames(thing.return) <- option.names
        rownames(thing.return) <- colnames(random.sample)
        for (qq in 1:nQ) {
          cur.table <- table(unlist(lapply(as.list( random.sample[,qq] ), function(x) strsplit(x, split = ""))))
          cur.table.names <- names(cur.table)
          cur.table.perc <- cur.table/sum(!is.na(random.sample[,qq]))
          for (rr in 1:length(cur.table.names)) {
            temp.loc <- match(cur.table.names[rr],option.names)
            res.freq.booted[ii,qq,temp.loc] = cur.table.perc[rr]
          }
        }
      }
      res.freq.mn <- round(apply(res.freq.booted,c(2,3),mean),3)
      colnames(res.freq.mn) <- option.names
      rownames(res.freq.mn) <- colnames(data.alpha)
      res.freq.sd <- round(apply(res.freq.booted,c(2,3),sd),4)
      colnames(res.freq.sd) <- option.names
      rownames(res.freq.sd) <- colnames(data.alpha)
      thing.return <- list()
      thing.return$res.freq.mn <- res.freq.mn
      thing.return$res.freq.sd <- res.freq.sd
      thing.return.master[[nn]] <- thing.return
      names(thing.return.master)[nn] <- noquote(paste0("respFreq.", temp.names[nn]))
    }
  }
  return(thing.return.master)
}


