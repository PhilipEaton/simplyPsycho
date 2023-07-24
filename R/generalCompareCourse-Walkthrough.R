#' General Comparison of Courses WALKTHROUGH:
#'
#' @description Walks the user through a general comparision of courses in the
#'  given sample.
#'
#' @param data Can be either
#'
#' 1) An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
#' where nS is the number of students in the sample and nQ is the number of questions,
#'
#' 2) The output of piql.data.select with one or multiple courses selected.
#'
#'
#' @return Cohen's d with interpretation, p-value and adjusted p-value of difference in means,
#' course normalized gain, difference in item difficulty with error. On request
#' box plots, change in difficulty plots, Sankey plots, and gains versus pre score plots
#' cal all be created. Sankey plots and gains versus pre score plots require matched data.
#'
#' NOTE: Use the Left and Right arrow in the Plots window to switch between the plots.
#'
#' @export
#'
#' @examples
#' # Pull sample data (*Current example data does not support this.*)
#' temp.data <- piql.data.select(simplySampleData, courses = 1, numBlanks.allowed = 0, MCMR.items = NA)
#'
#' # Begin walkthrough
#' gen.compare.courses.Walkthrough(temp.data)
gen.compare.courses.Walkthrough <- function (data){
  # Set up Flags
  flag.nRuns = FALSE
  flag.plotBoxPlots = FALSE
  flag.makeDiffPlots = FALSE
  flag.makeSankeyPlots = FALSE
  flag.return.gains = FALSE
  flag.makeGainsPlots = FALSE
  # ------------------------------------- #
  # Prompt for nRuns
  while (flag.nRuns == FALSE) {
    prompt.result <- NA
    cat("\n\n\n")
    prompt.result <- readline(prompt = cat("How many bootstrapping samples would you like to use? Must be a positive integer: "))
    if (is.na(suppressWarnings(as.numeric(prompt.result)))) {cat(crayon::cyan("Not a viable entry. Try again."))
    } else if ( (as.numeric(prompt.result) > 0) && (round(as.numeric(prompt.result),0) == as.numeric(prompt.result)) ) {
      flag.nRuns = TRUE
      nRuns <- as.numeric(prompt.result)
    } else {cat(crayon::cyan("Not a viable entry. Try again."))}
  }
  # ------------------------------------- #
  # Prompt for Plot Box Plots
  while (flag.plotBoxPlots == FALSE) {
    prompt.result <- NA
    cat("\n\n\n")
    prompt.result <- readline(prompt = cat("Would you like to display box plots of the total scores? (1 = Yes ; 2 = No) "))
    if (prompt.result == "1") {
      flag.plotBoxPlots = TRUE
      makeBoxPlots = TRUE
    } else if (prompt.result == "2") {
      flag.plotBoxPlots = TRUE
      makeBoxPlots = FALSE
    } else {cat(crayon::cyan("Not a viable entry. Try again."))}
  }
  # ------------------------------------- #
  # Prompt for Plot change in item difficulty
  while (flag.makeDiffPlots == FALSE) {
    prompt.result <- NA
    cat("\n\n\n")
    prompt.result <- readline(prompt = cat("Would you like to display plots of change in item difficulty? (1 = Yes ; 2 = No) "))
    if (prompt.result == "1") {
      flag.makeDiffPlots = TRUE
      makeDiffPlots = TRUE
    } else if (prompt.result == "2") {
      flag.makeDiffPlots = TRUE
      makeDiffPlots = FALSE
    } else {cat(crayon::cyan("Not a viable entry. Try again."))}
  }
  # ------------------------------------- #
  # Beyond this, data must be MATCHED
  if (data$Matched == TRUE) {
    # ------------------------------------- #
    # Prompt for Sankey Plots
    while (flag.makeSankeyPlots == FALSE) {
      prompt.result <- NA
      cat("\n\n\n")
      prompt.result <- readline(prompt = cat("Would you like to display Sankey plots for the total scores? (1 = Yes ; 2 = No) "))
      if (prompt.result == "1") {
        flag.makeSankeyPlots = TRUE
        makeSankeyPlots = TRUE
        flag.bins = FALSE
        while (flag.bins == FALSE) {
          prompt.result.bins <- readline(prompt = cat("How many total score bins would you like to use? (Must be a positive integer): "))
          if (is.na(suppressWarnings(as.numeric(prompt.result.bins)))) {cat(crayon::cyan("Not a viable entry. Try again."))
          } else if ( (as.numeric(prompt.result.bins) > 0) && (round(as.numeric(prompt.result.bins),0) == as.numeric(prompt.result.bins)) ) {
            flag.bins = TRUE
            nBins <- as.numeric(prompt.result.bins)
          } else {cat(crayon::cyan("Not a viable entry. Try again."))}
        }
      } else if (prompt.result == "2") {
        flag.makeSankeyPlots = TRUE
        makeSankeyPlots = FALSE
      } else {cat(crayon::cyan("Not a viable entry. Try again."))}
      # ------------------------------------- #
      # Prompt for Plotting Gains
      while (flag.makeGainsPlots == FALSE) {
        prompt.result <- NA
        cat("\n\n\n")
        prompt.result <- readline(prompt = cat("Would you like plot individual student normalized gains/changes versus pre score? (1 = Yes ; 2 = No) "))
        if (prompt.result == "1") {
          flag.makeGainsPlots = TRUE
          makeGainsPlots = TRUE
        } else if (prompt.result == "2") {
          flag.makeGainsPlots = TRUE
          makeGainsPlots = FALSE
        } else {cat(crayon::cyan("Not a viable entry. Try again."))}
      }
      # ------------------------------------- #
      # Prompt for Returning Gains values
      while (flag.return.gains == FALSE) {
        prompt.result <- NA
        cat("\n\n\n")
        prompt.result <- readline(prompt = cat("Would you like to receive individual student normalized gains/changes? (1 = Yes ; 2 = No) "))
        if (prompt.result == "1") {
          flag.return.gains = TRUE
          return.gains = TRUE
        } else if (prompt.result == "2") {
          flag.return.gains = TRUE
          return.gains = FALSE
        } else {cat(crayon::cyan("Not a viable entry. Try again."))}
      }
    }
  } else {
    makeSankeyPlots = FALSE
    nBins = 7
    return.gains = FALSE
    makeGainsPlots = FALSE
    }




  # ------------------------------------- #
  # Put it all together
  return(gen.compare.courses(data, nRuns = nRuns, makeBoxPlots = makeBoxPlots,
                             makeDiffPlots = makeDiffPlots,
                             makeSankeyPlots = makeSankeyPlots,
                             nBins = nBins,
                             return.gains = return.gains,
                             makeGainsPlots = makeGainsPlots))
}

