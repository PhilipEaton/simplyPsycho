#' Item Tree Analysis: Error Rate for minimized corrected ITA
#'
#' @description Calculates the minimized corrected error rate between the links carried in the current
#' tree and the contradiction matrix.
#'
#' @param current.tree Output from the itaForest() function. Current tree being considered.
#'
#' @param condradiction.mat Output from the contradiction.mat.generator() function.
#' Contradiction matrix for the data.
#'
#' @param p CTT Item Difficulty.
#'
#' @param nS Number of students in the data.
#'
#' @return Error rate for the current tree using the minimized corrected method.
#'
#' @export
#'

error.rate.minCor <- function(current.tree,condradiction.mat, p, nS) {
  nQ <- ncol(condradiction.mat)
  x <- rep(0, 4)
  for (ii in 1:nQ) {
    for (jj in 1:nQ) {
      if ( (sum(current.tree[current.tree[,1]==ii,2]==jj)>0) == TRUE &&
           ii != jj) {
        x[2] = x[2] + (-condradiction.mat[ii,jj]*p[jj]*nS)
        x[4] = x[4] + p[jj]^2*nS^2
      }
      if ( (sum(current.tree[current.tree[,1]==ii,2]==jj)>0) == FALSE &&
           (sum(current.tree[current.tree[,1]==jj,2]==ii)>0) == TRUE &&
           ii != jj) {
        x[1] = x[1] + (-condradiction.mat[ii,jj]*p[ii]*nS + p[ii]*p[jj]*nS^2 - p[ii]^2*nS^2)
        x[3] = x[3] + p[ii]^2*nS^2
      }
    }
  }
  return(-(x[1]+x[2])/(x[3]+x[4]))
}
