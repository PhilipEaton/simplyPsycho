#' CTT Discrimination WALKTHROUGH:
#'
#' @description Walks the user through the set up and use of calculating
#' Classical Test Theory item Discrimination for the items on an assessment
#' using a given sample.
#'
#' @param data Can be either
#'
#' 1) An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
#' where nS is the number of students in the sample and nQ is the number of questions,
#'
#' 2) The output of piql.data.select with one or multiple courses selected.
#'
#'
#' @return The CTT Discrimination analysis as specified in the walkthough
#'
#' @export
#'
#' @examples
#' # Pull in PIQL data from AWS and get some course data.
#' PIQLdata <- pullPIQLdata()
#' temp.data <- piql.data.select(PIQLdata, course = 2, numBlanks.allowed = 0)
#'
#' # Begin walkthrough
#' cttDiscrimination.Walkthrough(temp.data)
#'
cttDiscrimination.Walkthrough <- function (data){
  # Set up Flags
  flag.perc = FALSE
  flag.booted = FALSE
  flag.nRuns = FALSE
  flag.plotBarChart = FALSE
  # ------------------------------------- #
  # Prompt for perc
  while (flag.perc == FALSE) {
    prompt.result <- NA
    cat("\n\n\n")
    prompt.result <- readline(prompt = cat("What top/bottom percentage would you like to use? Must be >0 and <0.50. Typical value is 0.27: "))
    if (is.na(suppressWarnings(as.numeric(prompt.result)))) {cat(crayon::cyan("Not a viable entry. Try again."))
    } else if ( (as.numeric(prompt.result) > 0) && (as.numeric(prompt.result) < 0.50)  ) {
      flag.perc = TRUE
      perc <- as.numeric(prompt.result)
    } else {cat(crayon::cyan("Not a viable entry. Try again."))}
  }
  # ------------------------------------- #
  # Prompt for bootstrapping
  while (flag.booted == FALSE) {
    prompt.result <- NA
    cat("\n\n\n")
    prompt.result <- readline(prompt = cat("Would you like to estimate error using bootstrapping? (1 = Yes ; 2 = No) "))
    if (prompt.result == "1") {
      flag.booted = TRUE
      booted = TRUE
    } else if (prompt.result == "2") {
      flag.booted = TRUE
      booted = FALSE
    } else {cat(crayon::cyan("Not a viable entry. Try again."))}
  }
  # ------------------------------------- #
  # Prompt for nRuns
  if (booted == TRUE) {
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
  } else {nRuns <- 0}
  # ------------------------------------- #
  # Prompt for Plot Bar Chart
  while (flag.plotBarChart == FALSE) {
    prompt.result <- NA
    cat("\n\n\n")
    prompt.result <- readline(prompt = cat("Would you like to display a bar chart of the results? (1 = Yes ; 2 = No) "))
    if (prompt.result == "1") {
      flag.plotBarChart = TRUE
      plotBarChart = TRUE
    } else if (prompt.result == "2") {
      flag.plotBarChart = TRUE
      plotBarChart = FALSE
    } else {cat(crayon::cyan("Not a viable entry. Try again."))}
  }



  # ------------------------------------- #
  # Put it all together
  return(cttDiscrimination(data, perc, booted, nRuns, plotBarChart))
}

