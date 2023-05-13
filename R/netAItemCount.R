# The following walks the user through how to perform EFA on a sample set of data.
# Can be readilty adapted to be used on user's own data.
#
# Philip Eaton
#
#
# Get the items network in terms of the number of time two items are
# answered correctly or are selected by the same student
netA.item.count <- function(Data){
  thing <- igraph::graph.incidence(as.data.frame(Data))
  thing.pr <- igraph::bipartite.projection(thing)
  return(thing.pr$proj2)
}
