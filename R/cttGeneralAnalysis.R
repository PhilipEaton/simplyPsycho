#' CTT General Analysis
#'
#' @description Performs the following analysis: Cronbach's Alpha, Item point-biserial,
#' Item difficulty, and Item discrimination.
#'
#' @param data Can be either
#'
#' 1) An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
#' where nS is the number of students in the sample and nQ is the number of questions,
#'
#' 2) The output of piql.data.select with one or multiple courses selected.
#'
#' @param booted Logical (default = FALSE). FALSE means no bootstrapping will be
#' performed. TRUE turns on the bootstrapping feature.
#'
#' @param nRuns Number of random samples to use in the bootstrapping (default = 100).
#' For publications it is recommended that 10,000 runs be performed since sample
#' error goes as 1/sqrt(nRuns).
#'
#' @param plotBarChart (Default = FALSE). Generate barchart. If bootstrapping was
#' performed, then error bars wll be automatically added (+- 1 standard deviation).
#'
#' @return When booted = FALSE (the default setting) then the straight calculated
#' value will be returned. If booted = TRUE, the function will output the mean and
#' standard deviation for the CTT item difficulties calculated from nRuns randomly sampled
#' with replacement data sets from the given sample.
#'
#' @export
#'
#' @examples
#' # Pull in PIQL data from AWS and get some course data.
#' PIQLdata <- pullPIQLdata()
#' temp.piql.data <- PIQLdata$courses
#' data.num <- temp.data$data.num
#'
#' # Straight item difficulty.
#' cttDiff.mult(data.num)
#'
#' # Booted using 100 random samples (default)
#' cttDiff.mult(data.num, booted = TRUE)
#'
#' # Booted using 1000 random samples by manually setting nRuns
#' cttDiff.mult(data.num, booted = TRUE, nRuns = 1000)
#'
cttGeneralAnalysis <- function (data, perc = 0.27, booted = FALSE, nRuns = 100, plotBarChart = FALSE){
    if (typeof(data) == "list") {
      data <- data[[2]]
    }

    # Get number of student and questions
    nS <- nrow(data)
    nQ <- ncol(data)
    thing.return <- list()
    # Get Cronbach's Alpha
    thing.return$cronbahAlpha <- as.data.frame(cttCronbachAlpha(data, booted, nRuns))
    # Get item point-biserial
    thing.return$pointBi <- cttpointBiserial(data, booted, nRuns)
    # Get item difficulty
    thing.return$difficulty <- cttDiff(data, booted, nRuns)
    # Get item point-biserial
    thing.return$discrimination <- cttDisc(data, perc, booted, nRuns)

    # Plotting
    if (plotBarChart == TRUE) {
      error.bar <- function(x, y, upper, lower=upper, length=0.05,...){
        arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
      }

      layout(mat <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE),
             heights = c(0.75,1),
             widths  = c(1,1)
      )
      # Plot 1
      temp <- barplot(thing.return$cronbahAlpha[1,], col = c("lightblue"),
                      ylim = c(0,1), ylab = "Cronbach's Alpha", xlab = "Questions")
      abline(h = 0.70,col = "red")
      if (booted == TRUE) {error.bar(temp, thing.return$cronbahAlpha[1,], thing.return$cronbahAlpha[2,])}
      # Plot 2
      temp <- barplot(thing.return$pointBi[1,], col = c("lightblue"),
                      ylim = c(0,1), ylab = "Point-biserial", xlab = "Questions")
      abline(h = 0.20,col = "red")
      if (booted == TRUE) {error.bar(temp, thing.return$pointBi[1,], thing.return$pointBi[2,])}
      # Plot 3
      temp <- barplot(thing.return$difficulty[1,], col = c("lightblue"),
                      ylim = c(0,1), ylab = "Difficulty", xlab = "Questions")
      abline(h = c(0.20,0.80),col = "red")
      if (booted == TRUE) {error.bar(temp, thing.return$difficulty[1,], thing.return$difficulty[2,])}
      # Plot 4
      temp <- barplot(thing.return$discrimination[1,], col = c("lightblue"),
                      ylim = c(0,1), ylab = "Discrimination", xlab = "Questions")
      abline(h = c(0.30),col = "red")
      if (booted == TRUE) {error.bar(temp, thing.return$discrimination[1,], thing.return$discrimination[2,])}
      # Reset layout for future plots
      layout(mat = matrix(c(1), nrow = 1, ncol = 1),
             heights = c(1),widths  = c(1))
    }


  return(thing.return)
}

