#' Item Response theory 2-parameter logistic model with bootstrapping for error estimation:
#'
#' @description Calculate Item Response Theory (IRT) 2-parameter logistic model (2PL)
#' for given sample. Estimation of the error in item parameters can be found using
#' 2 methods.
#'
#' Method 1: Bootstrap the data. Estimate errors in item parameters by bootstrapping sample.
#' This given population level error estimations.
#'
#' Method 2: Randomly estimate the initial parameters in mirt. Estimate errors in
#' item parameters by wiggling the initial parameters. This given sample level error estimations.
#'
#' WARNING: IRT is in general a very computationally expensive process,
#' meaning it is slow. It is recommended you begin with 5 - 10 runs to get a feel for
#' what the data is doing, and then upping to a larger number of runs once you are
#' sure the output is giving you what you want.
#'
#' @param data.num An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
#' where nS is the number of students in the sample and nQ is the number of questions.
#'
#' @param Method (Default = 1) Method = 1 does traditional bootstrapping for
#' estimations of population-level error. Method = 2 does initial parameter randomization
#' for estimations of sample-level error.
#'
#' @param nRuns Number of random samples to use in the bootstrapping (default = 5).
#' For publications it is recommended that 10,000 runs be performed since sample
#' error goes as 1/sqrt(nRuns).
#'
#' @return Model fit and item parameter estimation mean and standard deviations for the sample.
#'
#'      $fit.means Model fit mean values from bootstrapping.
#'
#'      $fir.stDevs Model fit standard deviation values from bootstrapping.
#'
#'      $item.means Item parameter mean values from bootstrapping.
#'
#'      $item.stDevs Item parameter standard deviation values from bootstrapping.
#'
#' @export
#'
#' @examples
#' # Pull in PIQL data from AWS and get some course data.
#' PIQLdata <- pullPQLdata()
#' temp.piql.data <- PIQLdata$courses
#' data.alpha <- temp.data$data.alpha
#' data.num <- temp.data$data.num
#'
#' # Get irc data with default values.
#' irt2PL.Method1.res <- irt2PL(data.num)
#' irt2PL.Method1.res$fit.means
#' irt2PL.Method1.res$fit.stDevs
#' irt2PL.Method1.res$item.means
#' irt2PL.Method1.res$item.stDevs
#'
#' # Cange Method and number of runs
#' irt2PL.Method2.res <- irt2PL(data.num, Method = 2, nRuns = 10)
#' irt2PL.Method2.res$fit.means
#' irt2PL.Method2.res$fit.stDevs
#' irt2PL.Method2.res$item.means
#' irt2PL.Method2.res$item.stDevs
#'
irt2PL <- function (data.num, Method = 1, nRuns = 5) {
  nS <- nrow(data.num) # Get number of students in sample
  nQ <- ncol(data.num) # Get number of questions in sample
  total.scores.vec <- rowSums(data.num) # Get students' total scores.
  irc.fit.booted <- array(NA, dim = c(nRuns, 9))
  irc.itemStats.booted <- array(NA, dim = c(nRuns, nQ, 4))
  if (Method == 1) {
    ##################
    # Bootstrap the data.
    print("Estimate errors in item parameters by bootstrapping sample.")
    print("This given population level error estimations.")
    ##################
    for (ii in 1:nRuns) {
      print(paste0("Beginning run: ", ii, " of ", nRuns))
      # Draw a sample (with replacement) from the full sample
      ## that is the same size as the full sample.
      random.sample <- data.num[sample(1:nS,nS,replace = TRUE),]
      # Calculate and store disc
      temp.mirt <-  mirt::mirt(random.sample, 1, "2PL", technical = list(NCYCLES = 5000), verbose = FALSE)
      irc.fit.booted[ii, ] <- unlist(mirt::M2(temp.mirt))
      irc.itemStats.booted[ii,,] <- mirt::coef(temp.mirt, simplify = TRUE)$items
    }
  }

  if (Method == 2) {
    ##################
    # Randomly estimate the initial parameters in mirt.
    print("Estimate errors in item parameters by wiggling the initial parameters.")
    print("This given sample level error estimations.")
    ##################
    for (ii in 1:nRuns) {
      print(paste0("Beginning run: ", ii, " of ", nRuns))
      # Calculate and store disc
      temp.mirt <-  mirt::mirt(data.num, 1, "2PL", technical = list(NCYCLES = 5000), GenRandomPars = T, verbose = FALSE)
      irc.fit.booted[ii, ] <- unlist(mirt::M2(temp.mirt))
      irc.itemStats.booted[ii,,] <- mirt::coef(temp.mirt, simplify = TRUE)$items
    }
  }

  # Get the mean fit statistics
  irc.fit.boot.mn <- apply(irc.fit.booted, 2, mean)
  names(irc.fit.boot.mn) <- c("M2", "df", "p", "RMSEA", "RMSEA_5", "RMSEA_95", "SRMSR", "TLI", "CFI")
  # Get the St. Dev. of the fit statistics
  irc.fit.boot.sd <- apply(irc.fit.booted, 2, sd)
  # Get item parameter estimations
  irc.item.boot.mn <- apply(irc.itemStats.booted, c(2,3), mean)
  irc.item.boot.sd <- apply(irc.itemStats.booted, c(2,3), sd)
  # Format return
  thing.return <- list()
  thing.return$fit.means <- round(irc.fit.boot.mn,4)
  thing.return$fit.stDevs <- round(irc.fit.boot.sd,4)
  thing.return$item.means <- round(irc.item.boot.mn,4)
  thing.return$item.stDevs <- round(irc.item.boot.sd,4)
  return(thing.return)
}













