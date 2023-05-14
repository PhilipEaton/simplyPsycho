#' Item Tree Analysis - Cycle Killer
#'
#' @description TBA
#'
#' @return A plot after killing the detected cycle... hopefully
#'
#' @export
#'
#' @examples
#' # TBA

ita.cycle.killer <- function() {
  ## In the event a cycle is detected do the following:
  current.model <- iita.3[[1]]
  hasse.mat <- array(FALSE, dim = c(nQ, nQ))
  for (kk in 1:nrow(current.model)) {
    hasse.mat[current.model[kk,1],current.model[kk,2]] <- TRUE
  }
  ## the sum on columns gives the number of links going into the question
  colSums(hasse.mat)
  ## the sum on rows gives the number of links going out from the question
  rowSums(hasse.mat)

  # Find and eliminate connection going into an item with only one INTO connection
  # This will NORMALLY fix the cycle problem. If it doesn't, then repeat the
  ## process until it is fixed.
  temp.Qs <- c(1:nQ)[colSums(hasse.mat)==1]
  labs[temp.Qs]
  for (ii in temp.Qs) {
    current.model <- current.model[current.model[,2] != ii,]
  }
  hasse.mat <- array(FALSE, dim = c(nQ, nQ))
  for (kk in 1:nrow(current.model)) {
    hasse.mat[current.model[kk,1],current.model[kk,2]] <- TRUE
  }
  hasseDiagram::hasse(hasse.mat, labels = labs)
}
