#' Pooled Imputed results
#'
#' @description Takes in results from simplyPsycho functions applied to multiply imputed
#' data sets and returns the mean and sd of the results.
#'
#' @param to.be.pooled Must be the results of cttDifficulty, cttDiscrimination,
#' cttCronbachAlpha or cttpointBiserial.

#' @return Mean and sd of the results for multiple imputed data sets.
#'
#' @export
#'
#' @examples
#' # Pull sample data
#' temp.data <- piql.data.select(simplySampleData, courses = 1, numBlanks.allowed = 0)
#' answerKey <- simplySampleData$answerkey
#'
#' meh <- multiple.imputations(temp.data, answerKey)
#'
#' thing <- cttDifficulty(meh)
#' #thing <- cttDiscrimination(meh)
#' #thing <- cttCronbachAlpha(meh)
#' #thing <- cttpointBiserial(meh)

multiple.imputation.pooled <- function(to.be.pooled) {
  temp.names <- unique(names(to.be.pooled))

  thing.return <- list()
  for (nn in 1:length(temp.names)) {
    cur.name <- grep(temp.names[nn], names(to.be.pooled))
    array.data <- t(to.be.pooled[[((nn-1)*length(cur.name) + 1)]])
    for (ii in 2:length(cur.name)) {
      array.data <- cbind(array.data, t(to.be.pooled[[ii]]))
    }
    thing.return[[(2*nn - 1)]] <- round(apply(array.data, 1, mean),4)
    thing.return[[(2*nn)]] <- round(apply(array.data, 1, sd),4)
    names(thing.return)[(2*nn - 1) : (2*nn)] <- c( paste0(temp.names[nn], c(".mn", ".sd")))
  }
  return(thing.return)
}

