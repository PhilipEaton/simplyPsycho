#' Item Tree Analysis: Contradiction Matrix Generator
#'
#' @description Generates the contradiction matrix used in item tree analysis.
#' Finds the number of contradictions in the data to the assumption that
#' a correct response to item ii implies a correct response to item jj.
#' This is found by filtering for the students who all got ii correct,
#' then counting those who got jj incorrect.
#'
#' @param data An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
#' where nS is the number of students in the sample and nQ is the number of questions.
#'
#' @return Contradiction matrix to use used in ITA.
#'
#' @export
#'
#' @examples
#' # Pull sample data
#' temp.data <- piql.data.select(simplySampleData, courses = 1, numBlanks.allowed = 0, MCMR.items = NA)
#' data.alpha <- temp.data$data.alpha
#' data.num <- temp.data$data.num
#'
#' # Get contradiction matrix
#' contradiction.mat.generator(data.num)

contradiction.mat.generator <- function(data) {
  nQ <- ncol(data)
  contradiction.mat <- array(NA, dim = c(nQ,nQ))
  for (ii in 1:nQ) {
    for (jj in 1:nQ) {
      contradiction.mat[jj,ii] <- sum(data[data[,ii] == 1,jj] == 0)
    }
  }
  return(contradiction.mat)
}
