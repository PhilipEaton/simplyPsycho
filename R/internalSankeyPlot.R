#' Makes Sankey plot
#'
#' @description Makes Sankey plot for gen.compare.course
#'
#' @param data Can be either

#' @export
#'
#' @examples
#' # Pull in PIQL data from AWS and get some course data.
#' PIQLdata <- pullPIQLdata()
#' temp.data <- piql.data.select(PIQLdata, course = 2, numBlanks.allowed = 0)
#' data.num <- temp.data$data.num
general.compare.courses.MakeSankey <- function (set1, set2, nQ, nBins = 7,couse.names, ii, jj){
  full.sample <- cbind(set1, set2)
  nS <- nrow(full.sample) # Get number of students in sample
  num.scores <- (nQ+1)
  num.bins = nBins
  score.grouping <- array(0,dim = c(nS))
  top.of.bin <- numeric(num.bins+1)
  top.of.bin[1] <- nQ
  top.of.bin[num.bins+1] <- 0
  for (kk in 1:(num.bins-1)) {
    top.of.bin[kk+1] <- num.scores-1-kk*num.scores/num.bins
    score.grouping[full.sample[,1] <= (num.scores-1-kk*num.scores/num.bins)] = kk
  }
  score.grouping.100 <- score.grouping

  score.grouping <- array(0,dim = c(nS))
  for (kk in 1:(num.bins-1)) {
    score.grouping[full.sample[,2] <= (num.scores-1-kk*num.scores/num.bins)] = kk
  }
  score.grouping.200 <- score.grouping

  weight.mat <- array(NA, dim = c(num.bins,num.bins))
  for (kk in 1:num.bins) {
    for (hh in 1:num.bins) {
      weight.mat[kk,hh] <- sum((score.grouping.100==kk) & (score.grouping.200==hh))
    }
  }
  labels <- c(paste0(paste0(couse.names[ii],"."),paste0("[",top.of.bin[1:num.bins],"-",top.of.bin[2:(num.bins+1)],")")),
              paste0(paste0(couse.names[jj],"."),paste0("[",top.of.bin[1:num.bins],"-",top.of.bin[2:(num.bins+1)],")")))
  # Create a color palett that can be pulled from for a reasonable number of categories.
  cols <- c(colors()[1:num.bins],colors()[1:num.bins])
  sources <- sort(c(rep(c(1:num.bins),num.bins)))-1
  targets <- c(rep(c(1:num.bins),num.bins)) + num.bins-1
  weights <- as.vector(t(weight.mat))
  connector.cols <- as.matrix(array(NA, dim = c(num.bins, num.bins)))
  diag(connector.cols) = rgb(red = 0, green = 0, blue = 0, alpha = 0.5)
  connector.cols[upper.tri(connector.cols)] = rgb(red = 1, green = 0, blue = 0, alpha = 0.5)
  connector.cols[lower.tri(connector.cols)] = rgb(red = 0, green = 0, blue = 1, alpha = 0.5)
  connector.cols[2:num.bins,1] <- rgb(red = 0, green = 1, blue = 0, alpha = 0.5)
  connector.cols <- as.vector(t(connector.cols))

  fig <- plotly::plot_ly(
    type = "sankey",
    orientation = "h",

    node = list(
      line = list(color = "black",width = 0.5),
      label = labels,
      color = cols,
      pad = 15,
      thickness = 20
    ),

    link = list(
      source = sources,
      target = targets,
      value =  weights,
      color = connector.cols
    )
  )

  print(fig)
}

