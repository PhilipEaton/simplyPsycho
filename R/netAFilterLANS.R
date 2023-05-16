#' Locally Adaptive Network Sparcification
#'
#' @description Takes in an undirected network and performs LANS on it.
#'
#' @param initial.network Initial network of the data. Output from graph.adjacency() function.
#'
#' @param lans.alpha Cutoff value for the LANS sparcification procedure. Default is set to
#' 0.05, since this acts similar to a p-value.
#'
#' @return A LANS sparcified network.
#'
#' @export
#'
#' @examples
#' # Pull in PIQL data from AWS and get some course data.
#' PIQLdata <- pullPQLdata()
#' temp.piql.data <- PIQLdata$courses
#' data.num <- temp.data$data.num
#'
#' # Get LANS plot of data
#' netA.lans.plot(temp.data$data.num, cor.type = 1)
netA.Filter.LANS <- function(initial.network, lans.alpha) {
  # Get the weighted adjacency matrix
  LANS.network.edgematrix <- data.matrix(igraph::get.adjacency(initial.network, attr = "weight"))
  LANS.network.degree <- igraph::degree(initial.network)
  LANS.network.rowSum <- rowSums(abs(LANS.network.edgematrix))
  disparity.filter.marix <- array(0, dim = c(nrow(LANS.network.edgematrix),ncol(LANS.network.edgematrix)))
  Fstat.matrix <- array(0, dim = c(nrow(LANS.network.edgematrix),ncol(LANS.network.edgematrix)))
  for (ii in 1:nrow(LANS.network.edgematrix)) {
    for (jj in 1:ncol(LANS.network.edgematrix)) {
      disparity.filter.marix[ii,jj] <- abs(LANS.network.edgematrix[ii,jj])/LANS.network.rowSum[ii]
    }
  }
  for (ii in 1:nrow(LANS.network.edgematrix)) {
    for (jj in 1:ncol(LANS.network.edgematrix)) {
      Fstat.matrix[ii,jj] <- (sum(disparity.filter.marix[ii,jj] >= disparity.filter.marix[ii,])-1)/LANS.network.degree[ii]
    }
  }
  F.test.mat <- Fstat.matrix > (1 - lans.alpha)
  F.test.mat.sym <- F.test.mat + t(F.test.mat)
  edgeWeights <- LANS.network.edgematrix * (F.test.mat.sym > 0)
  lansFiltered.network <- igraph::graph_from_adjacency_matrix(edgeWeights, mode = "undirected", weighted = TRUE)
  return(lansFiltered.network)
}
