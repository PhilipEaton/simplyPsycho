#' Item Tree Analysis - Strictly Performed with bootstrapping
#'
#' @description Performs ITA using strict procedures with bootstrapping
#' This function is set up to consider every sensible tree by first considering
#' all possible connections in the assessment, then iteratively removing the worst
#' offending connection, one at a time, until no connections are left.
#' The returned model is the one which returns the smallest difference between
#' the estimated and original contradiction matrices.
#'
#' NOTE: The generally means a handful of questions get likely be removed entirely
#' from the tree due to high diff and error rates in the models that include them.
#'
#' SUGGESTION: A less strict version of this function is itaRelaxed and itxVeryRelaxed.
#'
#' @param data An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
#' where nS is the number of students in the sample and nQ is the number of questions.
#'
#' @param type (Default = 1) Type of ITA to be performed.
#'
#' Type = 1 - Original IITA
#'
#' Type = 2 - Corrected IITA
#'
#' Type = 3 - Minimized Corrected IITA
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
#' # Pull sample data
#' temp.data <- piql.data.select(simplySampleData, courses = 1, numBlanks.allowed = 0, MCMR.items = NA)
#' data.num.MCMR <- temp.data$data.num
#' # ITA with bootstrapping
#' ita.data.3.booted <- ita.strict.booted(data.num.MCMR, type = 3, nRuns = 3)
#'
#' ## PLOTTIING
#' # Set labels
#' labs <- c("Plants", "Miner", "Fish", "Hooke's Law", "Ferris",
#'           "Inv. g", "JogAB", "SphBottle", "mkp", "Slide", "Odometer",
#'           "Squareness", "Olive Oil", "Ch.Sph.", "Int. E", "Q&N", "Bhutan-A",
#'           "Bhutan-C", "Bhutan-D", "Work-D", "Work-G", "EField", "Delta v")
#'# Plot bootstrapped model
#'ita.Plot(ita.data.3.booted, labs = labs)

ita.strict.booted <- function (data, type = 1, retain.perc = 0.90, nRuns = 10, quite = FALSE) {
  nS <- nrow(data)
  nQ <- ncol(data)
  # Set up storage variables for the boot strapping:
  results.iita <- matrix(data.frame(), nrow = 3, ncol = nRuns)

  # Run the bootstrapping:
  for (runrun in 1:nRuns) {
    if (quite == FALSE) {
      print(paste0("Beginning run: ", runrun, " of ", nRuns))
    }
    # Draw a sample (with replacement) from the full sample
    ## that is the same size as the full sample.
    random.sample <- data[sample(1:nS,nS,replace = TRUE),]
    # Calculate and store iita
    results.iita[,runrun] <- ita.strict(random.sample,type)
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
