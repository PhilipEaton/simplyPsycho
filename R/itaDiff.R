#' Item Tree Analysis: Error in reconstructed condtradiction matrix
#'
#' @description Calculates error between the reconstructed and original
#' contradiction matrices. Goal in ITA is to get this to be as small as possible.
#'
#' @param contradiction.mat.orig Output from the contradiction.mat.generator() function.
#' Original contradiction matrix for the data.
#'
#' @param contradiction.mat.cur Current estimated contradication matrix. Calculated
#' differently depending on ITA method: Original, Corrected, or Monimized Corrected.
#'
#' @return The error (similar to a Chi^2) in the estimated contradiction matrix
#' for the current tree.
#'
#' @export
#'

diff.ita <- function(contradiction.mat.orig,contradiction.mat.cur) {
  nQ <- ncol(contradiction.mat.orig)
  return(1/(nQ*(nQ-1)) * sum( (contradiction.mat.orig - contradiction.mat.cur)^2 ) )
}
