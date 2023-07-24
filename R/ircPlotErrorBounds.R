#' Plot Item Response Curves with error bounds:
#'
#' @description Given the output from irc.get.Booted() function, create an IRC plot
#' with error bounds for a specified item.
#'
#' NOTE ABOUT WARNING MESSAGES: You will likely get a warning like "zero-length
#' arrow is of indeterminate angle and so skipped". This can be ignored as it
#' does not impact the plotting.
#'
#' @param irc.data.from.booted output from irc.get.Booted() function.
#'
#' @param qq Question number for the generated plot.
#'
#' @return IRC plot with error bounds for item number qq.
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
irc.plot.withErrorBounds <- function(irc.data.from.booted, qq) {
  irc.mn <- irc.data.from.booted[[1]]
  irc.sd <- irc.data.from.booted[[2]]
  Nq <- nrow(irc.mn[1,,])
  nO <- ncol(irc.mn[1,,])
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
  suppressWarnings(for (oo in 1:nO) {
    x.plt <- 0:Nq
    y.plt.min <- irc.mn[,qq,oo] - 1*irc.sd[,qq,oo]
    y.plt.max <- irc.mn[,qq,oo] + 1*irc.sd[,qq,oo]
    lines(x = 0:Nq, y =  y.plt.min, col = oo)
    lines(x = 0:Nq, y =  y.plt.max, col = oo)
    temp.errs <- rbind(c(x.plt,rev(x.plt)), c(y.plt.max,rev(y.plt.min)))
    temp.errs <- temp.errs[,!is.na(temp.errs[2,])]
    suppressWarnings(try(polygon(temp.errs[1,], temp.errs[2,], col = adjustcolor(oo, 0.4))))
   })
}
