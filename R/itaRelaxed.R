#' Item Tree Analysis - Relaxed
#'
#' @description Performs a relaxed ITA.
#' This function is set up to consider every sensible tree by first considering
#' all possible connections in the assessment, then iteratively removing the worst
#' offending connection, one at a time, until no connections are left.
#' The returned model is the one which returns the smallest difference between
#' the estimated and original contradiction matrices AND retains all of the items
#' on the assessment.
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
#' @return ITA map to be plotted using itaPlot() function.
#'
#'      $model Model found from analysis.
#'
#'      $diff Diff for the model.
#'
#'      $errorRate Error Rate for the model.
#'
#' @export
#'
#' @examples
#' # Get PIQL data
#' temp.data <- piql.data.select.MCMR(PIQLdata, course = 1)
#' data.alpha.MCMR <- temp.data$data.alpha
#' data.num.MCMR <- temp.data$data.num
#' # Check number of student removed. Should be less than 10%.
#' temp.data$nS.details
#'
#' # Original ITA
#' ita.data.1 <- ita.relaxed(data.num.MCMR,type = 1)
#' ita.data.1$model
#' ita.data.1$diff
#' ita.data.1$errorRate
#'
#' # Corrected ITA
#' ita.data.2 <- ita.relaxed(data.num.MCMR,type = 2)
#' ita.data.2$model
#' ita.data.2$diff
#' ita.data.2$errorRate
#'
#' # Minimized Corredted ITA
#' ita.data.3 <- ita.relaxed(data.num.MCMR,type = 3)
#' ita.data.3$model
#' ita.data.3$diff
#' ita.data.3$errorRate
#'
#' ## PLOTTIING
#' # Set labels
#' labs <- c("Plants", "Miner", "Fish", "Hooke's Law", "Ferris",
#' "Inv. g", "JogAB", "SphBottle", "mkp", "Slide", "Odometer",
#' "Squareness", "Olive Oil", "Ch.Sph.", "Int. E", "Q&N", "Bhutan-A",
#' "Bhutan-C", "Bhutan-D", "Work-D", "Work-G", "EField", "Delta v")
#'
#' # and plot
#' irt.Plot(ita.data.1$model, labs = labs)
#' irt.Plot(ita.data.2$model, labs = labs)
#' irt.Plot(ita.data.3$model, labs = labs)

ita.relaxed <- function(data, type = 1) {
  # Get average correct % for each question.
  ## Same thing as the CTT item difficulty
  p <- colMeans(data)
  nQ <- ncol(data)
  nS <- nrow(data)
  # find the number of contradictions to the assumption that
  ## a correction response to ii implies a correct response to jj.
  ## This is found but filtering for the students who all got ii correct,
  ## then counting who of those got jj incorrect.
  contradiction.mat.orig <- contradiction.mat.generator(data)


  # Put this into a list that better describes the relations:
  # "first" implies "second"
  # num.Con = number of contradictions
  map.contradictions <- data.frame(first = rep(c(1:nQ),nQ),
                                   second = sort(rep(c(1:nQ),nQ)),
                                   num.Con = unlist(as.list(contradiction.mat.orig)))
  map.contradictions <- map.contradictions[!(map.contradictions[,1]==map.contradictions[,2]),]
  # sort on smallest number of contradictions and then remove the self implications.
  ## i.e. question 1 implies questions 1, etc.
  map.contradictions <- map.contradictions[order(map.contradictions[,3]),]
  #map.contradictions <- map.contradictions[!(map.contradictions[,1]==map.contradictions[,2]),]
  # Open a list of what will hold all of the tree models
  map.contradictions.list <- matrix(data.frame(), nrow = nQ*(nQ-1), ncol = 1)
  diff.contradictions.list <- matrix(data.frame(), nrow = nQ*(nQ-1), ncol = 1)
  diff.ita.vec <- numeric(nQ*(nQ-1))
  error.rate.ita.vec <- numeric(nQ*(nQ-1))

  # Initialize the current map.
  map.contradictions.cur <- map.contradictions
  # Open the contradiction matrix for the current model
  contradiction.mat.cur <- array(NA, dim = c(nQ,nQ))
  for (mm in 1:(nQ*(nQ-1))) {
    # Find the diff for every model, removing the largest source of contradictions
    ## every loop.
    map.contradictions.list[[mm]] <- map.contradictions.cur
    #######
    # Original iita
    #######
    if (type == 1) {
      gamma.cur <- error.rate.orig(map.contradictions.cur,contradiction.mat.orig,p, nS)
      error.rate.ita.vec[mm] <- gamma.cur
      for (ii in 1:nQ) {
        for (jj in 1:nQ) {
          if ( sum(map.contradictions.cur[map.contradictions.cur[,1]==ii,2]==jj)>0 ) {
            contradiction.mat.cur[ii,jj] <- gamma.cur*p[jj]*nS
          } else {
            contradiction.mat.cur[ii,jj] <- (1-p[ii])*p[jj]*nS*(1-gamma.cur)
          }}}
    }
    #######
    # Corrected iita
    #######
    if (type == 2) {
      gamma.cur <- error.rate.orig(map.contradictions.cur,contradiction.mat.orig,p, nS)
      error.rate.ita.vec[mm] <- gamma.cur
      for (ii in 1:nQ) {
        for (jj in 1:nQ) {
          if ( sum(map.contradictions.cur[map.contradictions.cur[,1]==ii,2]==jj)>0 ) {
            contradiction.mat.cur[ii,jj] <- gamma.cur*p[jj]*nS
          } else {
            if (sum(map.contradictions.cur[map.contradictions.cur[,1]==jj,2]==ii)>0 ) {
              contradiction.mat.cur[ii,jj] <- (p[jj] - p[ii] + gamma.cur*p[ii])*nS
            } else {
              contradiction.mat.cur[ii,jj] <- (1-p[ii])*p[jj]*nS
            }}}}
    }
    #######
    # Minimized Corrected iita
    #######
    if (type == 3) {
      gamma.cur <- error.rate.minCor(map.contradictions.cur,contradiction.mat.orig, p, nS)
      error.rate.ita.vec[mm] <- gamma.cur
      for (ii in 1:nQ) {
        for (jj in 1:nQ) {
          if ( sum(map.contradictions.cur[map.contradictions.cur[,1]==ii,2]==jj)>0 ) {
            contradiction.mat.cur[ii,jj] <- gamma.cur*p[jj]*nS
          } else {
            if (sum(map.contradictions.cur[map.contradictions.cur[,1]==jj,2]==ii)>0 ) {
              contradiction.mat.cur[ii,jj] <- (p[jj] - p[ii] + gamma.cur*p[ii])*nS
            } else {
              contradiction.mat.cur[ii,jj] <- (1-p[ii])*p[jj]*nS
            }}}}
    }
    diag(contradiction.mat.cur) <- 0
    diff.contradictions.list[[mm]] <- contradiction.mat.cur
    # Calculate the difference between the estimated contradiction matrix and the
    ## actual contradiction matrix.
    diff.ita.vec[mm] <- diff.ita(contradiction.mat.orig,contradiction.mat.cur)
    # Now remove the worst offending link.
    ## WHILE maintaining all items in the model
    proposed.removal <- nrow(map.contradictions.cur)
    proposed.b.map <- map.contradictions.cur[-proposed.removal,]
    while (length(unique(c(proposed.b.map[,1],proposed.b.map[,2]))) < nQ &&
           proposed.removal > 1) {
      proposed.removal <- proposed.removal - 1
      proposed.b.map <- map.contradictions.cur[-proposed.removal,]
    }
    if (proposed.removal == 1) {
      break
    }
    map.contradictions.cur <- proposed.b.map
  }
  # If whole diff vec is not fillend then some 0s will be left over.
  ## Remove these before trying to find minimum.
  diff.ita.vec <- diff.ita.vec[diff.ita.vec != 0]
  results.iita <- matrix(data.frame(), nrow = 3, ncol = 1)
  # Format return
  results.iita <- list()
  results.iita$model <- map.contradictions.list[[match(min(diff.ita.vec), diff.ita.vec)]]
  results.iita$diff <- min(diff.ita.vec)
  results.iita$errorRate <- error.rate.ita.vec[match(min(diff.ita.vec), diff.ita.vec)]
  return(results.iita)
}


