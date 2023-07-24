#' Build simplyPsycho data from external data files. INTERNAL
#'
#' @description Build simplyPsycho data from external data files. INTERNAL
#'
build.psycho.datafile <- function (datafile, new.data, new.course.name = NULL, new.term = NULL, answerKey = NULL, gen.blanks = FALSE, blank.ID = NULL){
  # --------------------------------------------- #
  # Check if datafile is declared as a variable.
  ## Check that datafile is a list.
  if (typeof(datafile) != "list") {
      stop("Variable for the generated datafile must be a list.")}
  # --------------------------------------------- #
  # Give option of only putting in an answer key
  if (typeof(new.data) == "logical") {
    datafile$answerkey <- answerKey
  } else {
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
  }
  # --------------------------------------------- #
  # return new data file
  return(datafile)
}

