#' Item Tree Analysis - Full Control
#'
#' @description Performs an ITA where the user gets to control the max amount of
#' contractions between a causal pairing to be retained in the model.
#'
#' This file does item tree analysis with bootstrapping for error estimation:
#'  Relaxed the code to so that models will always
#'  1) includes all of the items on the instrument, and
#'  2) select the model with the best diff from those models.
#'
#' Not really recommend for publications boasting quantitative
#' robustness. This method is EXTREMELY adhoc and should only be used for
#' qualitative analysis purposes
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
#' @return ITA map to be plotted using itaPlot() function.
#'
#'      $model Model found from analysis.
#'
#'      $diff Diff for the model.
#'
#' @export
#'
#' @examples
#' # Pull sample data
#' temp.data <- piql.data.select(simplySampleData, courses = 1, numBlanks.allowed = 0, MCMR.grading = "Selected")
#' data.num.MCMR <- temp.data$data.num
#'
#' # Fully Controlled ITA
#' ita.data.fc <- ita.relaxed(data.num.MCMR)
#' ita.data.fc$model
#' ita.data.fc$diff
#'
#' ## PLOTTIING
#' # Set labels
#' labs <- c("Plants", "Miner", "Fish", "Hooke's Law", "Ferris",
#' "Inv. g", "JogAB", "SphBottle", "mkp", "Slide", "Odometer",
#' "Squareness", "Olive Oil", "Ch.Sph.", "Int. E", "Q&N", "Bhutan-A",
#' "Bhutan-C", "Bhutan-D", "Work-D", "Work-G", "EField", "Delta v")
#'
#' # and plot
#' irt.Plot(ita.data.fc$model, labs = labs)

ita.fullCon <- function(data, perc = 0.20, method = 1) {
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
  # sort on smallest number of contradictions and then remove the self implications.
  ## i.e. question 1 implies questions 1, etc.
  map.contradictions <- map.contradictions[order(map.contradictions[,3]),]
  map.contradictions <- map.contradictions[!(map.contradictions[,1]==map.contradictions[,2]),]


  # Remove implications that have more than perc * max contradictions:
  if (method == 1) {
    map.contradictions <- map.contradictions[map.contradictions[,3] <= perc * nS,]
  }
  if (method == 2) {
    map.contradictions <- map.contradictions[map.contradictions[,3] <= perc * max(map.contradictions[,3]),]
  }

  # Remove the worst offender in any transitive implications.
  thing <- c()
  for (kk in 1:nrow(map.contradictions)) {
    ii = map.contradictions[kk,1]
    jj = map.contradictions[kk,2]
    if ( (sum(map.contradictions[map.contradictions[,1]==jj,2]==ii)>0) == TRUE ) {
      test1 <- map.contradictions[kk,3]
      test2 <- map.contradictions[(map.contradictions[,1]==jj)+(map.contradictions[,2]==ii)==2,3]
      if (test1 <= test2) {
        thing <- c(thing,which(map.contradictions[,1] %in% jj)[which(map.contradictions[,1] %in% jj) %in% which(map.contradictions[,2] %in% ii)])
      } else {
        thing <- c(thing,kk)
      }
    }
  }
  if (!is.null(thing)) {
    map.contradictions <- map.contradictions[-thing,]
  }
  # Return the resulting map
  # Format return
  results.iita <- list()
  results.iita$model <- map.contradictions
  results.iita$diff <- diff.ita(contradiction.mat.orig,map.contradictions)
  return(results.iita)
}

