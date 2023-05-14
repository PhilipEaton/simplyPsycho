#' Item Tree Analysis: Error Rate for original and corrected ITA
#'
#' @description Calculates the error rate between the links carried in the current
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
#' @return Error rate for the current tree.
#'
#' @export
#'

error.rate.orig <- function(current.tree, condradiction.mat, p, nS) {
  thing <- 0
  for (kk in 1:nrow(current.tree)) {
    ii <- current.tree[kk,1]
    jj <- current.tree[kk,2]
    thing <- thing + condradiction.mat[ii,jj]/(p[jj]*nS)
  }
  return(thing/(nrow(current.tree)))
}
