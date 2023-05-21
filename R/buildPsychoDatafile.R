#' CTT Difficulty of multiple courses with bootstrapping for error estimation and plotting:
#'
#' @description Calculate Classical Test Theory item difficulty for the items on
#' an assessment using a given sample.
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

# $courses
# $answerkey
# $courseList
# $term
build.psycho.datafile <- function (datafile, new.data, new.course.name = NULL, new.term = NULL, answerKey = NULL, gen.blanks = FALSE, blank.ID = NULL){
  # --------------------------------------------- #
  # Check if datafile is declared as a variable.
  ## Check that datafile is a list.
  if (typeof(datafile) != "list") {
      stop("Variable for the generated datafile must be a list.")}
  # --------------------------------------------- #
  # Get columns names of new.data to do testing
  check.col.names <- colnames(new.data)
  # Get list names of datafile to do testing
  check.datafile.names <- names(datafile)
  # --------------------------------------------- #
  # Check that new.data is formatted properly.
  # newdata needs to be a data frame
  if (is.data.frame(new.data)==FALSE) {stop("Data must be entered as a data frame.")}
  # Check that Student.Code exits
  if ( ("Student.Code" %in% check.col.names) == FALSE) {stop("Data must have a column labled: Student.Code")}
  # Check that question columns begin with Q
  Qcols <- substr(check.col.names,1,1) == "Q"
  if (sum(Qcols) == 0) {stop("Question columns need to begin with Q, like 'Q1' or 'Q12'")}
  # Check that blanks exists
  if ( (("blanks" %in% check.col.names) == FALSE) && (gen.blanks == FALSE) ) {
    stop("Data does not have a blanks column. Either add that, or set gen.blanks = TRUE")}
  if ( (("blanks" %in% check.col.names) == FALSE) && (gen.blanks == TRUE) ) {
    if ( is.null(blank.ID) == TRUE ) {stop("Cannot generate blanks unless blank.ID is specified.")}
      nS <- nrow(new.data)
      temp <- numeric(nS)
      for (ii in 1:nS) {
        temp[ii] <- sum(new.data[ii,Qcols] %in% blank.ID)
      }
      # add blanks column
      new.data$blanks <- temp
    }
  # --------------------------------------------- #
  # Get number of courses in datafile
  ## If if doesn't exist, then open it and set new course number
  ## If it does, get new course number
  if (("courseList" %in% check.datafile.names) == FALSE) {
    newCourseNum <- 1
    datafile$courseList <- c() } else {newCourseNum <- length(datafile$courseList) + 1}
  # --------------------------------------------- #
  # Update courses
  datafile$courses[[newCourseNum]] <- new.data
  # --------------------------------------------- #
  # Attach the given answer key.
  if ( is.null(answerKey) ) {print("No answer key give.")
  } else {datafile$answerkey <- answerKey}
  # --------------------------------------------- #
  # Update courseList
  ## If no course name is given, then use course number
  if ( is.null(new.course.name) == TRUE ){
    datafile$courseList <- c(datafile$courseList, newCourseNum)
  } else {datafile$courseList <- c(datafile$courseList, new.course.name)}
  # --------------------------------------------- #
  # Update term
  ## If if doesn't exist, then open it
  if (("term" %in% check.datafile.names) == FALSE) {
    datafile$term <- c() }
  ## Add term to list
  ## If no course name is given, then use course number
  if ( is.null(new.term) == TRUE ){
    datafile$term <- c(datafile$term, NA)
  } else {datafile$term <- c(datafile$term, new.term)}
  # --------------------------------------------- #
  # return new data file
  return(datafile)
}

