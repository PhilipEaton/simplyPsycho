#' Community membership data for plotting
#'
#' @description Generates community membership data using multiple network analysis
#' community detection methods and bootstrapping.
#'
#' @param data.num An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
#' where nS is the number of students in the sample and nQ is the number of questions.
#'
#' @param cor.type (default = 1). 1 = Pearson r, 2 = polychoric, 3 = partial correlation.
#'
#' @param lans.alpha Cutoff value for the LANS sparcification procedure. Default is set to
#' 0.05, since this acts similar to a p-value.
#'
#' @param nRuns Number of random samples to use in the bootstrapping (default = 100).
#' For publications it is recommended that 10,000 runs be performed since sample
#' error goes as 1/sqrt(nRuns).
#'
#' @param printRunNumber Default = FALSE. Print the run number currently being computed.
#' Useful when doing long runs and you want to keep track of there the code is.
#'
#' @return Community membership data to be put unto netA.Generate.PairedMat()
#' for the generation of heatmaps.
#'
#' @export
#'
#' @examples
#' # Pull in PIQL data from AWS and get some course data.
#' PIQLdata <- pullPIQLdata()
#' temp.piql.data <- PIQLdata$courses
#' data.num <- temp.data$data.num
#'
#' # Get community membership data
#' data.memberships <- netA.Community.Memberships(temp.data$data.num)
#' # Generate the pared matrix
#' data.PairedMat <- netA.Generate.PairedMat(thing.memberships$fastgreedy)
#' # Create heat map
#' heatmap(data.PairedMat)
#'
netA.Community.Memberships <- function (data.num, cor.type = 1, lans.alpha = 0.05, nRuns = 100, printRunNumber = FALSE) {
  nQ <- ncol(data.num)
  nS <- nrow(data.num)
  ########################
  # Now bootstrap sample
  #  and get community measures:
  ########################
  #Select type of correlation you want to use:
  # 1 = Pearson r
  # 2 = Polychoric
  # 3 = Partial Correlation
  # How many samplings do you want to do? (1000 is a good)
  alpha <- lans.alpha
  membership.booted <- array(NA, dim = c(nRuns, nQ, 6))

  for (ii in 1:nRuns) {
    if (printRunNumber == TRUE) {
      cat("Begin Run: ", ii, " of ", nRuns, ". \n", sep="")
    }
    # Draw a sample (with replacement) from the full sample
    ## that is the same size as the full sample.
    random.sample <- data.num[sample(1:nS,nS,replace = TRUE),]
    if (cor.type == 1) {
      na.edge.boot <- cor(random.sample)
    } else if (cor.type == 2) {
      na.edge.boot <- psych::polychoric(random.sample)$rho
    } else if (cor.type == 3) {
      na.edge.boot <- psych::partial.r(random.sample)
    }
    #na.edge.boot <- polychoric(data.num)$rho
    #na.edge.data <- partial.r(data.num)
    diag(na.edge.boot) <- 0 # remove self connections
    graph.boot <- igraph::graph.adjacency(abs(na.edge.boot), weighted = TRUE, mode = "max")
    lansFiltered.boot <- netA.Filter.LANS(graph.boot, alpha)
    # Calculate and store memberships
    membership.booted[ii,,1] <-  igraph::cluster_fast_greedy(lansFiltered.boot, weights = abs(igraph::E(lansFiltered.boot)$weight))$membership
    membership.booted[ii,,2] <-  igraph::cluster_optimal(lansFiltered.boot, weights = abs(igraph::E(lansFiltered.boot)$weight))$membership
    membership.booted[ii,,3] <-  igraph::cluster_walktrap(lansFiltered.boot, weights = abs(igraph::E(lansFiltered.boot)$weight))$membership
    membership.booted[ii,,4] <-  igraph::cluster_label_prop(lansFiltered.boot, weights = abs(igraph::E(lansFiltered.boot)$weight))$membership
    membership.booted[ii,,5] <-  igraph::cluster_leiden(lansFiltered.boot, weights = abs(igraph::E(lansFiltered.boot)$weight))$membership
  }
  # Format return
  thing.return <- list()
  thing.return$fastgreedy <- membership.booted[,,1]
  thing.return$optimal <- membership.booted[,,1]
  thing.return$walktrap <- membership.booted[,,1]
  thing.return$labelprop <- membership.booted[,,1]
  thing.return$leiden <- membership.booted[,,1]
  return(thing.return)
}


