#' Plot Item Response Curves with error bars:
#'
#' @description Given the output from irc.get.Booted() function, create an IRC plot
#' with error bars for a specified item.
#'
#' @param irc.data.from.booted output from irc.get.Booted() function.
#'
#' @param qq Question number for the generated plot.
#'
#' @return IRC plot with error bars for item number qq.
#'
#' @export
#'
#' @examples
#' # Pull sample data
#' temp.data <- piql.data.select(simplySampleData, courses = 1, numBlanks.allowed = 0, MCMR.items = NA)
#' data.alpha <- temp.data$data.alpha
#' data.num <- temp.data$data.num
#'
#' # Get irc data with default values.
#' irc.data.booted <- irc.get.Booted(data.alpha,data.num)

#' # Plot item 2.
#' irc.plot.withErrorBars(irc.data.booted, 2)
#' # Plot item 4.
#' irc.plot.withErrorBounds(irc.data.booted, 4)

irc.plot.withErrorBars <- function(irc.data.from.booted, qq) {
  irc.mn <- irc.data.from.booted[[1]]
  irc.sd <- irc.data.from.booted[[2]]
  Nq <- nrow(irc.mn[1,,])
  irc.mn[irc.mn==0] <- NA
  plot( y = irc.mn[1,qq,], x = rep(0, length(irc.mn[1,qq,])),
        xlim = c(-1,(Nq+1)), ylim = c(0,1.05),
        pch = c("A", "B", "C", "D","E", "F", "G", "H"), col = 1:8,
        #main = paste0("Question ",qq),
        xlab = NA,
        ylab = NA)
  title(paste0("Question ",qq), line = 0.5, cex.main = 1.5)
  abline(h=0)
  for (ii in 1:(Nq+1)) {
    points(y = irc.mn[ii,qq,], x = rep((ii-1), length(irc.mn[ii,qq,])),
           pch = c("A", "B", "C", "D","E", "F", "G", "H"), col = 1:8)
  }
  suppressWarnings(for (ii in 1:(Nq+1)) {
    try(arrows(x0=rep((ii-1), length(irc.mn[ii,qq,])),
           y0=irc.mn[ii,qq,]-irc.sd[ii,qq,],
           x1=rep((ii-1), length(irc.mn[ii,qq,])),
           y1=irc.mn[ii,qq,]+irc.sd[ii,qq,],
           code=3, angle=90, length=0.1,
           col = 1:8))
  })
}
