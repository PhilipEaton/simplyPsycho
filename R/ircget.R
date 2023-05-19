#' Item Response Curve data generator:
#'
#' @description Calculates the plotting data needed to create item response curves
#' for the items on an assessment using a given sample.
#'
#' @param data.Alpha An nS by nQ matrix or data frame of ALPHABETICAL data,
#' where nS is the number of students in the sample and nQ is the number of questions.
#'
#' @param data.Num An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
#' where nS is the number of students in the sample and nQ is the number of questions.
#'
#' @param nO The maximum number of options available on the assessment. Default it set to
#' find this value automatically given the response options present in the sample.
#' It is suggested you enter than value manually if the max number of options is
#' known. For example,  if you know item 7 has 6 options, the most out of all the
#' items on the assessment, then set nO = 7.
#'
#' @return Plotting data for item response curves. To be put into the ircPlot() function.
#'
#' @export
#'
#' @examples
#' # Pull in PIQL data from AWS and get some course data.
#' PIQLdata <- pullPIQLdata()
#' temp.piql.data <- PIQLdata$courses
#' data.alpha <- temp.data$data.alpha
#' data.num <- temp.data$data.num
#'
#' # get irc data and plot item 2
#' irc.data <- irc.get(data.alpha, data.num)
#' irc.plot(irc.data, 2)
irc.get <- function(data.Alpha, data.Num, nO = NULL) {
  nQ <- ncol(data.Alpha)
  if (is.null(nO)) {
    nO <- length(table(unlist(as.list(data.Alpha))))
  }
  total.score.vec <- rowSums(data.Num)
  irc.data <- array(NA, dim = c((nQ+1), nQ, nO))
  for (qq in 1:nQ) {
    for (ss in 1:(nQ+1)) {
      if (sum(total.score.vec == (ss-1))>0) {
        temp.options <- data.Alpha[total.score.vec == (ss-1),qq]
        cur.table <- table(c(unlist(strsplit(temp.options, split = ""))))
        cur.table.names <- match(names(cur.table), LETTERS)
        cur.table.perc <- cur.table/sum(total.score.vec == (ss-1))
        for (rr in cur.table.names) {
          irc.data[ss,qq,rr] = cur.table.perc[match(rr,cur.table.names)]
        }
      }}
  }
  return(irc.data)
}
