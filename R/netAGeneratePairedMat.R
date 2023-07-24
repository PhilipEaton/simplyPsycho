#' Create paired matrix for network analysis community detection heat maps
#'
#' @description Generates paired matrix from community membership data.
#'
#' @param memberMatrix Output from netA.Community.Memberships() function.
#'
#' @return Paired matrix from community membership data to be plotted using heatmap().
#'
#' @export
#'
#' @examples
#' # Pull sample data
#' temp.data <- piql.data.select(simplySampleData, courses = 1, numBlanks.allowed = 0, MCMR.items = NA)
#' data.num <- temp.data$data.num
#'
#' # Get community membership data
#' data.memberships <- netA.Community.Memberships(temp.data$data.num)
#' # Generate the pared matrix
#' data.PairedMat <- netA.Generate.PairedMat(thing.memberships$fastgreedy)
#' # Create heat map
#' heatmap(data.PairedMat)
#'
netA.Generate.PairedMat <- function(memberMatrix) {
  pairedMat <- matrix(0, nrow = ncol(memberMatrix), ncol = ncol(memberMatrix))
  foo <- nrow(memberMatrix)
  for (i in 1:ncol(memberMatrix)) {
    for (j in i:ncol(memberMatrix)) {
      pairedMat[i,j] <- (sum(memberMatrix[,i] == memberMatrix[,j]) - sum(memberMatrix[,i] + memberMatrix[,j] == 0))/foo
    }
  }
  pairedMat <- pairedMat + t(pairedMat)
  diag(pairedMat) = 1
  return(pairedMat)
}
