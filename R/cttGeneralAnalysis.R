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
#' @param perc (Default = 0.27) percentage or percentile to be used in the analysis.
#'
#' @param as.Percentile (Default = FALSE) If true, then students are split on percentile
#' not as percentage in total score.
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
#' # Pull sample data
#' temp.data <- piql.data.select(simplySampleData, courses = 1, numBlanks.allowed = 0, MCMR.items = NA)
#' data.num <- temp.data$data.num.111
#'
#' # Input can be the score matrix
#' cttGeneralAnalysis(data.num)
#' # or the output from piql.data.select()
#' cttGeneralAnalysis(temp.data)
#' # Supports Bootstrapping of all statistics
#' cttGeneralAnalysis(data.num, booted = TRUE)
#' # And will plot the results (with error bars if bootstrapped)
#' cttGeneralAnalysis(data.num, plotBarChart = TRUE)
#' cttGeneralAnalysis(data.num, booted = TRUE, plotBarChart = TRUE)
#'
cttGeneralAnalysis <- function (data, perc = 0.27, as.Percentile = FALSE, booted = FALSE, nRuns = 100, plotBarChart = FALSE){
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

  # Get number of courses and questions
  nC <- length(temp.nums)
  nQ <- ncol(data.list[[1]])
  # Open storage
  thing.return.master <- list()
  plotting.info.ca <- array(NA, dim = c(nC,2))
  plotting.info.pb <- array(NA, dim = c(2*nC,nQ))
  colnames(plotting.info.pb) <- colnames(data.list[[1]])
  plotting.info.diff <- array(NA, dim = c(2*nC,nQ))
  colnames(plotting.info.diff) <- colnames(data.list[[1]])
  plotting.info.disc <- array(NA, dim = c(2*nC,nQ))
  colnames(plotting.info.disc) <- colnames(data.list[[1]])
  for (nn in 1:nC) {
    data <- data.list[[nn]]
    # Get number of questions
    nS <- nrow(data.list[[1]])
    thing.return <- list()
    # ANALYSIS
    # Get Cronbach's Alpha
    thing.return$cronbahAlpha <- t(cttCronbachAlpha(data, booted, nRuns)[[1]])
    if (nC == 1) {plotting.info.ca[nn,1] <- thing.return$cronbahAlpha[1]
    } else {plotting.info.ca[nn,1] <- thing.return$cronbahAlpha[1]}
    rownames(thing.return$cronbahAlpha) <- paste0(rownames(thing.return$cronbahAlpha),".",temp.names[nn])
    # Get item point-biserial
    thing.return$pointBi <- cttpointBiserial(data, booted, nRuns)[[1]]
    plotting.info.pb[(2*nn-1),] <- thing.return$pointBi[1,]
    rownames(thing.return$pointBi) <- paste0(rownames(thing.return$pointBi),".",temp.names[nn])
    # Get item difficulty
    thing.return$difficulty <- cttDifficulty(data, booted, nRuns)[[1]]
    plotting.info.diff[(2*nn-1),] <- thing.return$difficulty[1,]
    rownames(thing.return$difficulty) <- paste0(rownames(thing.return$difficulty),".",temp.names[nn])
    # Get item point-biserial
    thing.return$discrimination <- as.data.frame(cttDiscrimination(data, perc, as.Percentile, booted, nRuns)[[1]])
    plotting.info.disc[(2*nn-1),] <- unlist(thing.return$discrimination[1,1:nQ])
    rownames(thing.return$discrimination) <- paste0(rownames(thing.return$discrimination),".",temp.names[nn])
    # Save plotting information if booted = TRUE
    if (booted == TRUE) {
      plotting.info.ca[nn,2] <- thing.return$cronbahAlpha[2]
      plotting.info.pb[(2*nn),]   <- thing.return$pointBi[2,]
      plotting.info.diff[(2*nn),]   <- thing.return$difficulty[2,]
      plotting.info.disc[(2*nn),]   <- unlist(thing.return$discrimination[2,1:nQ])
    }
    if (nn == 1) {thing.return.master <- thing.return
    } else {
      thing.return.master$cronbahAlpha <- rbind(thing.return.master$cronbahAlpha,thing.return$cronbahAlpha)
      thing.return.master$pointBi <- rbind(thing.return.master$pointBi,thing.return$pointBi)
      thing.return.master$difficulty <- rbind(thing.return.master$difficulty,thing.return$difficulty)
      thing.return.master$discrimination <- rbind(thing.return.master$discrimination,thing.return$discrimination)
    }

  }



  # Plotting
  if (plotBarChart == TRUE) {
    error.bar <- function(x, y, upper, lower=upper, length=0.05,...){
      arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
    }

    save.par <- par()
    par(mar = c(3.1, 4.1, 1, 1) )
    layout(mat <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE),
           heights = c(1,1),
           widths  = c(1,1)
    )
    if (nC == 1) {cols <- "lightblue"
    } else {cols <- safe_colorblind_palette[c(1:nC)]}
    # Plot 1
    temp <- barplot(plotting.info.ca[,1], col = cols, beside = TRUE,
                    ylim = c(0,1), xaxt='n')
    temp.label.locations <- temp
    axis(1, at = temp.label.locations, labels = temp.names, cex.axis = 1.2, padj = -0.5)
    title(ylab = "Cronbach's Alpha", line = 2.25, cex.lab=1.2)
    abline(h = 0.70,col = "red")
    if (booted == TRUE) {error.bar(temp, plotting.info.ca[,1], plotting.info.ca[,2])}
    # Plot 2
    temp <- barplot(plotting.info.pb[2*c(1:nC)-1,], col = cols,
                    ylim = c(0,1), beside = TRUE, xaxt='n')
    if (dim(temp)[2] == 1) { temp <- t(temp)  }
    temp.label.locations <- temp[ceiling(nrow(temp)/2),]
    axis(1, at = temp.label.locations, labels = colnames(plotting.info.pb), cex.axis = 0.75, padj = -1)
    title(xlab = "Questions", line = 2, cex.lab=1.2)
    title(ylab = "Point-biserial", line = 2.25, cex.lab=1.2)
    abline(h = 0.20,col = "red")
    if (booted == TRUE) {error.bar(temp, plotting.info.pb[2*c(1:nC)-1,], plotting.info.pb[2*c(1:nC),])}
    # Plot 3
    temp <- barplot(plotting.info.diff[2*c(1:nC)-1,], col = cols,
                    ylim = c(0,1), beside = TRUE, xaxt='n')
    if (dim(temp)[2] == 1) { temp <- t(temp)  }
    temp.label.locations <- temp[ceiling(nrow(temp)/2),]
    axis(1, at = temp.label.locations, labels = colnames(plotting.info.diff), cex.axis = 0.75, padj = -1)
    title(xlab = "Questions", line = 2, cex.lab=1.2)
    title(ylab = "Difficulty", line = 2.25, cex.lab=1.2)
    abline(h = c(0.20,0.80),col = "red")
    if (booted == TRUE) {error.bar(temp, plotting.info.diff[2*c(1:nC)-1,], plotting.info.diff[2*c(1:nC),])}
    # Plot 4
    temp <- barplot(plotting.info.disc[2*c(1:nC)-1,], col = cols,
                    ylim = c(0,1), beside = TRUE, xaxt='n')
    if (dim(temp)[2] == 1) { temp <- t(temp)  }
    temp.label.locations <- temp[ceiling(nrow(temp)/2),]
    axis(1, at = temp.label.locations, labels = colnames(plotting.info.disc), cex.axis = 0.75, padj = -1)
    title(xlab = "Questions", line = 2, cex.lab=1.2)
    title(ylab = "Discrimination", line = 2.25, cex.lab=1.2)
    abline(h = c(0.30),col = "red")
    if (booted == TRUE) {error.bar(temp, plotting.info.disc[2*c(1:nC)-1,], plotting.info.disc[2*c(1:nC),])}
    # Reset layout for future plots
    par(mar = c(5.1, 4.1, 4.1, 2.1) )
    layout(mat = matrix(c(1), nrow = 1, ncol = 1),
           heights = c(1),widths  = c(1))
  }


  return(thing.return.master)
}

