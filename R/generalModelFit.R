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
#' skew, kertosis and standard error will be estimated for the given course(s).
#' Model = "Bimodal" will also report the possible location of the two mean values
#' assuming the sample is drawn from a bimodal distribution.
#' Model = "Johnson" will use a Johnson fit method to fit the data including the skew and
#' kurtosis.
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
#' # Pull sample data
#' temp.data <- piql.data.select(simplySampleData, courses = 1, numBlanks.allowed = 0)
#' data.num <- temp.data$data.num
#' # --------------------------- #
#' # For multiple courses
#' data.multiple <- piql.data.select(simplySampleData, courses = c(1,2), numBlanks.allowed = 0, Matched = TRUE)
#'
#' ## Single Course
#' # Total score means and standard deviations...
#' gen.model.TotalScores(data.num)
#' # ...  with optional plotting.
#' gen.model.TotalScores(data.num, makePlot = TRUE)
#' # Supports bimodal estimations as well
#' gen.model.TotalScores(data.num, model = "Bimodal", makePlot = TRUE)
#'
#' # Total score means and standard deviations...
#' gen.model.TotalScores(data.multiple)
#' # ...  with optional plotting.
#' gen.model.TotalScores(data.multiple, makePlot = TRUE)
#' # Supports bimodal estimations as well
#' gen.model.TotalScores(data.multiple, model = "Bimodal", makePlot = TRUE)

gen.model.TotalScores <- function(data, model = "Normal", makePlot = FALSE){
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
  grid <- seq(0, nQ, length = (nQ * 5) + 1)
  plotting.info.normals <- array(0, dim = c(length(temp.nums), (nQ * 5) + 1))
  plotting.info.normal.bimodal <- array(0, dim = c(length(temp.nums), 6))
  plotting.info.counts <- array(0, dim = c(length(temp.nums), (nQ+1)))
  params.main <- list()
  for (nn in 1:length(temp.nums)) {
    data <- data.list[[nn]]
    totalScores <- rowSums(data, na.rm = TRUE)
    nS <- length(totalScores)
    test1 <- stats::shapiro.test(totalScores)$p.value
    test2 <- nortest::lillie.test(totalScores)$p.value
    meanScore <- mean(totalScores)
    sdScore <- sd(totalScores)
    std_mean <- function(x) sd(x)/sqrt(length(x))
    # Get normal distribution for plotting
    plotting.info.counts[nn,] = table(cut(totalScores, breaks = seq(0,(nQ+1), by = 1), include.lowest = TRUE, right = FALSE))/length(totalScores)
    plotting.info.normals[nn,] = dnorm(grid, meanScore, sdScore)
    # Store return information
    if (model == "Normal") {
      temp.moments <- SuppDists::moments(totalScores)
      thing.return.master[[nn]] <- data.frame(mean = temp.moments[1], stDev = temp.moments[2], std.error = std_mean(totalScores),
                                              skew = temp.moments[3], kurtosis = temp.moments[4],
                                              norm.test.shapiro = test1, norm.test.lillie = test2)
      names(thing.return.master)[nn] <- noquote(paste0("course", temp.names[nn]))
    }
    # Store return information
    if (model == "Johnson") {
      temp.moments <- SuppDists::moments(totalScores)
      thing.return.master[[nn]] <- data.frame(mean = temp.moments[1], stDev = temp.moments[2], std.error = std_mean(totalScores),
                                              skew = temp.moments[3], kurtosis = temp.moments[4],
                                              norm.test.shapiro = test1, norm.test.lillie = test2)
      names(thing.return.master)[nn] <- noquote(paste0("course", temp.names[nn]))
      params.main[[nn]] <- SuppDists::JohnsonFit(totalScores, moment="quant")
    }
    if (model == "Bimodal") {
      #thing <- suppressWarnings(multimode::locmodes(totalScores, mod0=2,display = FALSE))
      temp.bimodal.results <- mixtools::normalmixEM(totalScores, maxit = 5000)
      plotting.info.normal.bimodal [nn,c(1,2)] <- temp.bimodal.results$mu
      plotting.info.normal.bimodal [nn,c(3,4)] <- temp.bimodal.results$sigma
      plotting.info.normal.bimodal [nn,c(5,6)] <- temp.bimodal.results$lambda
      thing.return.master[[nn]] <- data.frame(mean = meanScore, stDev = sdScore, std.error = std_mean(totalScores),
                                              mean1.est = temp.bimodal.results$mu[1], mean2.est = temp.bimodal.results$mu[2],
                                              norm.test.shapiro = test1, norm.test.lillie = test2)
      names(thing.return.master)[nn] <- noquote(paste0("course", temp.names[nn]))
    }

  }
  # PLOTTING
  if (makePlot == TRUE) {
    ymax = max(c(max(plotting.info.normals), max(plotting.info.counts)))
    plot(x = c(0:nQ), y = plotting.info.counts[1,], pch = 16, col = 1,
         ylim = c(0,1.1*ymax), xaxt='n', ylab = NA, xlab = NA)
    axis(1, at = 0:nQ, labels = 0:nQ, cex.axis = 1, padj = -1)
    title(xlab = "Total Scores", line = 2, cex.lab=1.2)
    title(ylab = "Fraction of Students", line = 2, cex.lab=1.2)
    if (model == "Normal") {
      title(main = "Normal Distribution", line = 2, cex.main=1.5)
      lines(x = grid, y = plotting.info.normals[1,], col = 1)
    } else if (model == "Bimodal") {
      title(main = "Bimodal Distribution", line = 2, cex.main=1.5)
      abline(v = thing.return.master[[1]][c(4,5)], col = 1, lty = c(2,3))
      means <- plotting.info.normal.bimodal[1,c(1,2)]
      dev <- plotting.info.normal.bimodal[1,c(3,4)]
      prop <- plotting.info.normal.bimodal[1,c(5,6)]
      plotting.info.normal.1 <- dnorm(grid, mean = means[1], sd = dev[1])
      plotting.info.normal.2 <- dnorm(grid, mean = means[2], sd = dev[2])
      lines(x = grid, y = prop[1]*plotting.info.normal.1 + prop[2]*plotting.info.normal.2, lty = 1)
      lines(x = grid, y = prop[1]*plotting.info.normal.1, lty = 2)
      lines(x = grid, y = prop[2]*plotting.info.normal.2, lty = 3)
    } else if (model == "Johnson") {
      title(main = "Johnson Fit", line = 2, cex.main=1.5)
      parms <- params.main[[1]]
      lines(x = grid, y = SuppDists::dJohnson(grid, parms), col = 1)
    }
    if (length(temp.nums) > 1) {
      for (nn in 2:length(temp.nums)) {
        points(x = c(0:nQ), y = plotting.info.counts[nn,], pch = 16, col = safe_colorblind_palette[nn-1])
        if (model== "Normal") {
          lines(x = grid, y = plotting.info.normals[nn,], col = safe_colorblind_palette[nn-1])
        } else if (model == "Bimodal") {
          abline(v = thing.return.master[[nn]][c(4,5)], lty = c(2,3), col = safe_colorblind_palette[nn-1])
          means <- plotting.info.normal.bimodal[nn,c(1,2)]
          dev <- plotting.info.normal.bimodal[nn,c(3,4)]
          prop <- plotting.info.normal.bimodal[nn,c(5,6)]
          plotting.info.normal.1 <- dnorm(grid, mean = means[1], sd = dev[1])
          plotting.info.normal.2 <- dnorm(grid, mean = means[2], sd = dev[2])
          lines(x = grid, y = prop[1]*plotting.info.normal.1 + prop[2]*plotting.info.normal.2, col = safe_colorblind_palette[nn-1], lty = 1)
          lines(x = grid, y = prop[1]*plotting.info.normal.1, col = safe_colorblind_palette[nn-1], lty = 2)
          lines(x = grid, y = prop[2]*plotting.info.normal.2, col = safe_colorblind_palette[nn-1], lty = 3)
        } else if (model == "Johnson") {
          parms <- params.main[[nn]]
          lines(x = grid, y = SuppDists::dJohnson(grid, parms), col = safe_colorblind_palette[nn-1])
        }
      }
      legend("topright", inset = 0.01, legend=c(temp.names), col = c(1, safe_colorblind_palette[2:length(temp.names)]),
             lty = 1, cex=1, box.lty=0, ncol = length(temp.names))
    }
  }
  # return
  return(thing.return.master)
}
