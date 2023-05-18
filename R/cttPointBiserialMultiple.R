#' CTT Point-biserial:
#'
#' @description Calculates the point-biserial correlation between the total score
#' (with focus item removed) and the focus item. This gives a measure if the items
#' on the assessment are all measuring a single similar latent trait. If the point-biserial
#' is poor, then this could be an indication that the assessment is multidimensional.
#'
#' @param booted Logical (default = FALSE). FALSE means no bootstrapping will be
#' performed. TRUE turns on the bootstrapping feature.
#'
#' @param nRuns Number of random samples to use in the bootstrapping (default = 100).
#' For publications it is recommended that 10,000 runs be performed since sample
#' error goes as 1/sqrt(nRuns).
#'
#'
#' @param data An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
#' where nS is the number of students in the sample and nQ is the number of questions.
#'
#' @param plotBarChart (Default = FALSE). Generate barchart. If bootstrapping was
#' performed, then error bars wll be automatically added (+- 1 standard deviation).
#'
#' @return Point-biserial correlation between the total score (with focus item removed)
#' and the focus item. For all items on the assessment
#'
#' @export
#'
#' @examples
#' # Pull in PIQL data from AWS and get some course data.
#' PIQLdata <- pullPIQLdata()
#' temp.piql.data <- PIQLdata$courses
#' data.num <- temp.data$data.num
#'
#' # Point-biserial
#' cttpointBiserial.multi(data.num)
cttpointBiserial.multi <- function(data, booted = FALSE, nRuns = 100, plotBarChart = FALSE){
    data.list <- list()

    if (typeof(data) == "list") {
      temp.nums <- grep("data.num", names(data))
      temp.names <- substr(names(data)[temp.nums],10,12)
      for (nn in 1:length(temp.nums)) {
        data.list[[nn]] <- data[[ temp.nums[nn] ]]
      }
    }

    if (typeof(data) == "double") {
      temp.nums <- 1
      temp.names <- ""
      data.list[[1]] <- data
    }

    thing.return.master <- list()
    for (nn in 1:length(temp.nums)) {
      data <- data.list[[nn]]
      # Get number of student and questions
      nS <- nrow(data)
      nQ <- ncol(data)

      thing.return.master[[nn]] <- cttpointBiserial(data, booted, nRuns = nRuns)
      names(thing.return.master)[nn] <- noquote(paste0("pointBi", temp.names[nn]))
    }

    error.bar <- function(x, y, upper, lower=upper, length=0.05,...){
      arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
    }

    if (plotBarChart == TRUE) {
      if (length(thing.return.master) == 1) {
        temp <- barplot(thing.return.master[[1]][1,], col = c("lightblue"),
                        ylim = c(0,1), ylab = "CTT point-biserial", xlab = "Questions")
        if (booted == TRUE) {
          error.bar(temp, thing.return.master[[1]][1,], thing.return.master[[1]][2,])
        }
      }
      if (length(thing.return.master) > 1) {
        plot.thing <- thing.return.master[[1]][1,]
        if (booted == TRUE) {
          plot.thing.eb <- thing.return.master[[1]][2,]
        }
        for (mm in 2:length(thing.return.master)) {
          plot.thing <- rbind(plot.thing, thing.return.master[[mm]][1,])
          if (booted == TRUE) {
            plot.thing.eb <- rbind(plot.thing.eb, thing.return.master[[mm]][2,])
          }
        }
        temp <- barplot(plot.thing, col = c(1:length(thing.return.master)) + 1, beside = TRUE,
                        ylim = c(0,1), ylab = "CTT point-biserial", xlab = "Questions")
        if (booted == TRUE) {
          error.bar(temp, as.matrix(plot.thing), as.matrix(plot.thing.eb))
        }
      }
    }
  return(thing.return.master)
}
