#' General fitting of total scores with a normal distribution.
#'
#' @description Fits a normal distribution to the total scores.
#'
#' @param data Can be either
#'
#' 1) An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
#' where nS is the number of students in the sample and nQ is the number of questions,
#'
#' 2) The output of piql.data.select with one or multiple courses selected.
#'
#' @param model (Default = "Normal") Model = "Normal", the mean, standard deviation,
#' and standard error will be estimated for the given course(s). Model = "Bimodal"
#' will also report the possible locations of the maximums (means) assuming a
#' bimodal distribution.
#'
#' @param makePlot (Default = FALSE). If TRUE, then a single plot of the
#' fraction of students with each possible total score and the estimated normal
#' distribution for each courses in the data will be created.
#'
#' @return Mean, Standard deviation, and optional plot. Additionally, if model = "Bimodal"
#' then the estimated locaitons of maximums (means) assuming a bimodal distribution will be
#' reported.
#'
#' @export
#'
#' @examples
#' # Pull in PIQL data from AWS and get some course data.
#' PIQLdata <- pullPIQLdata()
#' # --------------------------- #
#' # Get PIQL data for one course
#' temp.data <- piql.data.select(PIQLdata, course = 2, numBlanks.allowed = 0)
#' data.alpha <- temp.data$data.alpha
#' data.num <- temp.data$data.num
#' # --------------------------- #
#' # For multiple courses
#' data.multiple <- piql.data.select(PIQLdata, course = c(1,2,123))
#'
#' ## Single Course
#' # Total score means and standard deviations...
#' gen.model.TotalScores(data.num)
#' # ...  with optional plotting.
#' gen.model.TotalScores(data.num, makePlot = TRUE)
#' # Supports bimodal estimations as well
#' gen.model.TotalScores(data.num, model = "Bimodal", makePlot = TRUE)
#'
#' ## Multiple courses
#' # Total score means and standard deviations...
#' gen.model.TotalScores(data.multiple)
#' # ...  with optional plotting.
#' gen.model.TotalScores(data.multiple, makePlot = TRUE)
#' # Supports bimodal estimations as well
#' gen.model.TotalScores(data.multiple, model = "Bimodal", makePlot = TRUE)
gen.model.TotalScores <- function(data, model = "normal", makePlot = FALSE){
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
  nQ <- ncol(data.list[[1]]) # Get number of questions
  plotting.info.normals <- array(0, dim = c(length(temp.nums), (nQ * 5) + 1))
  plotting.info.counts <- array(0, dim = c(length(temp.nums), (nQ+1)))
  grid <- seq(0, nQ, length = (nQ * 5) + 1)
  for (nn in 1:length(temp.nums)) {
    data <- data.list[[nn]]
    totalScores <- rowSums(data, na.rm = TRUE)
    test1 <- stats::shapiro.test(totalScores)$p.value
    test2 <- nortest::lillie.test(totalScores)$p.value
    meanScore <- mean(totalScores)
    sdScore <- sd(totalScores)
    std_mean <- function(x) sd(x)/sqrt(length(x))
    # Get normal distribution for plotting
    plotting.info.counts[nn,] = table(cut(totalScores, breaks = seq(0,(nQ+1), by = 1), include.lowest = TRUE, right = FALSE))/length(totalScores)
    plotting.info.normals[nn,] = dnorm(grid, meanScore, sdScore)
    # Store return information
    if (model == "normal") {
      thing.return.master[[nn]] <- data.frame(mean = meanScore, stDev = sdScore, std.error = std_mean(totalScores),
                                              norm.test.shapiro = test1, norm.test.lillie = test2)
      names(thing.return.master)[nn] <- noquote(paste0("course", temp.names[nn]))
    }
    if (model == "Bimodal") {
      thing <- suppressWarnings(multimode::locmodes(totalScores, mod0=2,display = FALSE))
      thing.return.master[[nn]] <- data.frame(mean = meanScore, stDev = sdScore, std.error = std_mean(totalScores),
                                              mode1.est = thing$locations[1], mode2.est = thing$locations[3],
                                              norm.test.shapiro = test1, norm.test.lillie = test2)
      names(thing.return.master)[nn] <- noquote(paste0("course", temp.names[nn]))
    }

  }

  # PLOTTING
  if (makePlot == TRUE) {
    ymax = max(c(max(plotting.info.normals), max(plotting.info.counts)))
      plot(x = c(0:nQ), y = plotting.info.counts[1,], pch = 16, col = 1,
           ylim = c(0,1.1*ymax), ylab = "Fraction of Students", xlab = "Total Scores")
      lines(x = grid, y = plotting.info.normals[1,], col = 1)
      if (model == "Bimodal") {
        abline(v = thing.return.master[[1]][c(4,5)], col = 1, lty = c(1,2))
      }
      if (length(temp.nums) > 1) {
        for (nn in 2:length(temp.nums)) {
          points(x = c(0:nQ), y = plotting.info.counts[nn,], pch = 16, col = nn)
          lines(x = grid, y = plotting.info.normals[nn,], col = nn)
          if (model == "Bimodal") {
            abline(v = thing.return.master[[nn]][c(4,5)], col = nn, lty = c(1,2))
          }
        }
      }
  }
  # return
  return(thing.return.master)
}
