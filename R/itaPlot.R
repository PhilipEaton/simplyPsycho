#' Item Tree Analysis - Create a Plot
#'
#' @description Creates a plot given a ITA tree
#'
#' @param current.tree The tree to be plotted
#'
#' @param labs Labels for the tree, if any.
#'
#' @return Hesse Plot of the current tree.
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
#' ita.data.1 <- ita.strict(data.num.MCMR,type = 1)
#' ita.data.1$model
#' ita.data.1$diff
#' ita.data.1$errorRate
#'
#' # Corrected ITA
#' ita.data.2 <- ita.strict(data.num.MCMR,type = 2)
#' ita.data.2$model
#' ita.data.2$diff
#' ita.data.2$errorRate
#'
#' # Minimized Corredted ITA
#' ita.data.3 <- ita.strict(data.num.MCMR,type = 3)
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

ita.Plot <- function(current.tree, labs = NULL) {
  # Check to see if Rgraphviz is in the library listing. If not, install it.
  if ( ("Rgraphviz" %in% rownames(installed.packages())) == FALSE ) {
    install.packages("BiocManager")
    BiocManager::install("Rgraphviz")
  }
  if (is.null(labs)) {
    nQ <- length(unique(c(current.tree[,1], current.tree[,2])))
  } else {
    nQ <- length(labs)
    # Check that lengths line up
    if (nQ != max(c(current.tree[,1], current.tree[,2])) ) {
      warning("Length of labs and the number of items in the tree are not the same! Some items are likely
              missing from your tree.")
    }
  }

  hasse.mat <- array(FALSE, dim = c(nQ, nQ))
  for (kk in 1:nrow(current.tree)) {
    hasse.mat[current.tree[kk,1],current.tree[kk,2]] <- TRUE
  }
  hasseDiagram::hasse(hasse.mat, labels = labs)
}

