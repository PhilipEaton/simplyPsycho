#' Item Tree Analysis - Full Control with bootstrapping
#'
#' @description Performs an ITA where the user gets to control the max amount of
#' contractions between a causal pairing to be retained in the model.
#'
#' @param data An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
#' where nS is the number of students in the sample and nQ is the number of questions.
#'
#' @param perc (Default = 0.20) Percentage of either the number of students or the
#' max number of contradictions to be retained in the model.
#'
#' @param method (Defait = 1) Method 1 is based on the number of students and will be
#' the same for every bootstrapped model. Method 2 is based on the maximum number
#' on contradictions in the initial model and can vary a little between random samples.
#'
#' @param retain.perc (Default = 0.90) Retain the links that are present in this percentage of
#' all the bootstrapped runs.
#'
#' @param nRuns Number of random samples to use in the bootstrapping (default = 10).
#' For publications it is recommended that 10,000 runs be performed since sample
#' error goes as 1/sqrt(nRuns).
#'
#' @return ITA map resulting from bootstrapping to be plotted using itaPlot() function.
#'
#' @export
#'
#' @examples
#' # Get PIQL data
#' temp.data <- piql.data.select.MCMR(PIQLdata, course = 1)
#' data.num.MCMR <- temp.data$data.num
#' # ITA with bootstrapping
#' ita.data.fc.booted <- ita.fullCon.booted(data.num.MCMR, type = 3, nRuns = 3)
#'
#' ## PLOTTIING
#' # Set labels
#' labs <- c("Plants", "Miner", "Fish", "Hooke's Law", "Ferris",
#'           "Inv. g", "JogAB", "SphBottle", "mkp", "Slide", "Odometer",
#'           "Squareness", "Olive Oil", "Ch.Sph.", "Int. E", "Q&N", "Bhutan-A",
#'           "Bhutan-C", "Bhutan-D", "Work-D", "Work-G", "EField", "Delta v")
#'# Plot bootstrapped model
#'ita.Plot(ita.data.fc.booted, labs = labs)

ita.fullCon.booted <- function (data, perc = 0.20, method = 1, retain.perc = 0.90, nRuns = 10, quite = FALSE) {
  nS <- nrow(data)
  nQ <- ncol(data)
  # Set up storage variables for the boot strapping:
  results.iita <- matrix(data.frame(), nrow = 2, ncol = nRuns)

  # Run the bootstrapping:
  for (runrun in 1:nRuns) {
    if (quite == FALSE) {
      print(paste0("Beginning run: ", runrun, " of ", nRuns))
    }
    # Draw a sample (with replacement) from the full sample
    ## that is the same size as the full sample.
    random.sample <- data[sample(1:nS,nS,replace = TRUE),]
    # Calculate and store iita
    results.iita[,runrun] <- ita.fullCon(random.sample, perc, method)
  }


  # Set up all possible implications:
  map.contradictions.results <- data.frame(first = rep(c(1:nQ),nQ),
                                           second = sort(rep(c(1:nQ),nQ)),
                                           num.Con = numeric(length(rep(c(1:nQ),nQ))))
  # Tally the number of times an implication was included in the iita
  for (rr in 1:nRuns) {
    for (kk in 1:nrow(map.contradictions.results)) {
      ii <- map.contradictions.results[kk,1]
      jj <- map.contradictions.results[kk,2]
      map.contradictions.results[kk,3] <- map.contradictions.results[kk,3] + sum(results.iita[,rr][[1]][results.iita[,rr][[1]][,1] == ii,2]==jj)
    }
  }
  # Removes implications that were never used
  map.contradictions.results <- map.contradictions.results[map.contradictions.results[,3] != 0,]
  # Sort into descending order
  map.contradictions.results <- map.contradictions.results[order(map.contradictions.results[,3], decreasing = TRUE),]
  # Retain the pairs there were present for, say, retain.perc% of the runs
  map.contradictions.results <- map.contradictions.results[map.contradictions.results[,3] >= retain.perc*nRuns,]
  # Results
  return(map.contradictions.results)
}
