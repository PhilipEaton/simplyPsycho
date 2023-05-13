# The following walks the user through how to perform EFA on a sample set of data.
# Can be readilty adapted to be used on user's own data.
#
# Philip Eaton
#
#
netA.Community.HeatMap <- function (data.num, cor.type = 1, lans.alpha = 0.05) {

  ########################
  # Now bootstrap sample
  #  and get community measures:
  ########################
  #Select type of correlation you want to use:
  # 1 = Pearson r
  # 2 = Polychoric
  # 3 = Partial Correlation
  cor.type <- 1
  # How many samplings do you want to do? (1000 is a good)
  nRuns <- 100
  alpha <- 0.05
  membership.booted <- array(NA, dim = c(nRuns, nQ, 6))

  for (ii in 1:nRuns) {
    cat("Begin Run: ", ii, " of ", nRuns, ". \n", sep="")
    # Draw a sample (with replacement) from the full sample
    ## that is the same size as the full sample.
    random.sample <- data.num[sample(1:nS,nS,replace = TRUE),]
    if (cor.type == 1) {
      na.edge.boot <- cor(random.sample)
    } else if (cor.type == 2) {
      na.edge.boot <- polychoric(random.sample)$rho
    } else if (cor.type == 3) {
      na.edge.boot <- partial.r(random.sample)
    }
    #na.edge.boot <- polychoric(data.num)$rho
    #na.edge.data <- partial.r(data.num)
    diag(na.edge.boot) <- 0 # remove self connections
    graph.boot <- graph.adjacency(abs(na.edge.boot), weighted = TRUE, mode = "max")
    lansFiltered.boot <- LANSnetwork.Filter(graph.boot, alpha)
    # Calculate and store memberships
    membership.booted[ii,,1] <-  cluster_fast_greedy(lansFiltered.boot, weights = abs(E(lansFiltered.boot)$weight))$membership
    membership.booted[ii,,2] <-  cluster_optimal(lansFiltered.boot, weights = abs(E(lansFiltered.boot)$weight))$membership
    membership.booted[ii,,3] <-  cluster_walktrap(lansFiltered.boot, weights = abs(E(lansFiltered.boot)$weight))$membership
    membership.booted[ii,,4] <-  cluster_label_prop(lansFiltered.boot, weights = abs(E(lansFiltered.boot)$weight))$membership
    membership.booted[ii,,5] <-  cluster_leiden(lansFiltered.boot, weights = abs(E(lansFiltered.boot)$weight))$membership
  }

  # CLuster: Fast and Greedy
  heatmap(genPairedMax(membership.booted[,,1]))
  # CLuster: Optimal
  heatmap(genPairedMax(membership.booted[,,2]))
  # CLuster: Walktrap
  heatmap(genPairedMax(membership.booted[,,3]))
  # CLuster: Label Propogation
  heatmap(genPairedMax(membership.booted[,,4]))
  # CLuster: Leiden
  heatmap(genPairedMax(membership.booted[,,5]))


}








