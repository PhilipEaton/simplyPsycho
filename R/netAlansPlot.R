#' Locally Adaptive Network Sparcification Plot
#'
#' @description Takes in an undirected network and performs LANS on it.
#'
#' @param data.num An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
#' where nS is the number of students in the sample and nQ is the number of questions.
#'
#' @param cor.type (default = 1). 1 = Pearson r, 2 = polychoric, 3 = partial correlation.
#'
#' @param lans.alpha Cutoff value for the LANS sparcification procedure. Default is set to
#' 0.05, since this acts similar to a p-value.
#'
#' @param makePlot Generate plot or not.
#'
#' @return A plot of the LANS sparcified network for the given data.
#'
#' @export
#'
#' @examples
#' # Pull in PIQL data from AWS and get some course data.
#' temp.data <- piql.data.select(simplySampleData, courses = 1, numBlanks.allowed = 0)
#' data.num <- temp.data$data.num
#'
#' # Get LANS plot of data
#' netA.lans.plot(temp.data$data.num, cor.type = 1)
netA.lans.plot <- function (data.num, cor.type = 1, lans.alpha = 0.05, makePlot = TRUE) {

  ##################
  #      Correlation matrix
  # Note: You can change the kind of correlation:
  #                     Pearson r
  #                     Polychoric
  #                     Partial Correlation
  # By changing the cor() function rght before the adjacency matrix is calculated.
  ##################
  nS <- nrow(data.num)
  nQ <- ncol(data.num)

  ####################
  # Full Data Analysis
  ####################
  #Select type of correlation you want to use:
  # 1 = Pearson r
  # 2 = Polychoric
  # 3 = Partial Correlation
  if (cor.type == 1) {
    na.edge.data <- cor(data.num)
  } else if (cor.type == 2) {
    na.edge.data <- psych::polychoric(data.num)$rho
  } else if (cor.type == 3) {
    na.edge.data <- psych::partial.r(data.num)
  }
  diag(na.edge.data) <- 0 # remove self connections
  initial.network <- igraph::graph.adjacency(abs(na.edge.data), weighted = TRUE, mode = "max")

  #################
  # LANS Filtering
  #   plus plotting
  #################
  # LANS will retain the alpha% strongest edges for *each* node
  ## Note: a weak edge can be retained for node A if it is in the top alpha% for
  ## another node connecting onti node A.
  lansFiltered.network <- netA.Filter.LANS(initial.network, lans.alpha)

  # Check how thinned out the graph is after LANS filtering.
  ## Density = 1 means all connections possible are in the graph.
  ## Density = 0.20 means only 20% of all possible connections are in the graph.
  initial.density <- igraph::graph.density(initial.network) # the network before LANS filtering
  lans.density <- igraph::graph.density(lansFiltered.network) # the network after LANS filtering

  # Get and display all the measures of centrality for the LANS filtered network
  centrality.table <- round(data.frame(degree = igraph::degree(lansFiltered.network),
                                       closeness =  igraph::closeness(lansFiltered.network),
                                       betweenness =  igraph::betweenness(lansFiltered.network),
                                       evcent = igraph::evcent(lansFiltered.network)$vector),3)

  # Plot filtered network
  if (makePlot == TRUE) {
    plot(lansFiltered.network,
         vertex.size = 15,
         vertex.label.cex = 0.8,
         edge.width=abs(igraph::E(lansFiltered.network)$weight)*10,
         edge.color = ifelse(igraph::E(lansFiltered.network)$weight > 0, "blue","red"),
         main = paste0("LANS Filtered Network (alpha = ",lans.alpha,")"))
  }

  thing.return <- list()
  thing.return$initial.density <- initial.density
  thing.return$lans.density <- lans.density
  thing.return$centrality <- centrality.table
  return(thing.return)
}
