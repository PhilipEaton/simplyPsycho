#' Item Response Curve plotter:
#'
#' @description Plots the result of irc.get().
#'
#' @param irc.data Output from irc.get().
#'
#' @param qq Question number you would like plotted.
#'
#' @return IRC plot for the requested item using the given irc.get() output.
#'
#' @export
#'
#' @examples
#' # Pull sample data
#' temp.data <- piql.data.select(simplySampleData, courses = 1, numBlanks.allowed = 0)
#' data.alpha <- temp.data$data.alpha
#' data.num <- temp.data$data.num
#'
#' # get irc data and plot item 2
#' irc.data <- irc.get(data.alpha, data.num)
#' irc.plot(irc.data, 2)
irc.plot <- function(irc.data, qq) {
  Nq <- nrow(irc.data[1,,])
  irc.data[irc.data==0] <- NA
  plot( y = irc.data[1,qq,], x = rep(0, length(irc.data[1,qq,])),
        xlim = c(-1,(Nq+1)), ylim = c(0,1.05),
        pch = c("A", "B", "C", "D","E", "F", "G", "H"), col = 1:8,
        #main = paste0("Question ",qq),
        xlab = NA,
        ylab = NA)
  title(paste0("Question ",qq), line = 0.5, cex.main = 1.5)
  abline(h=0)
  for (ii in 1:(Nq+1)) {
    points(y = irc.data[ii,qq,], x = rep((ii-1), length(irc.data[ii,qq,])),
           pch = c("A", "B", "C", "D","E", "F", "G", "H"), col = 1:8)
  }
}
