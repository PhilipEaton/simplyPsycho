#' Select specific course from the pulled PIQL data.
#'
#' @description Selects a specific course from the pulled PIQL data.
#'
#' @param pulled.PIQL.data Output from pullPQLdata
#'
#' @param course (Default = 1 = 121) Course number (1 = 121, 2 = 122, 3 = 123, etc.) or position in course list:
#'
#' Course: 121, 122, 123, 141, 142, 143, 224, 225, 226, 321, 322, 323, 325
#'
#' *Can be a list of courses.*
#'
#' @param numBlanks.allowed (default = 0) Number of blanks allowed in the resulting data.
#'
#' @return A list of course data for the PIQL stored in AWS.
#'
#'      $data.alpha ALPHABETICAL data for the selected course.
#'
#'      $data.num NUMERICAL data for the selected course.
#'
#'      $nS.details Details about how many students were removed due to the specified numBlanks.allowed value.
#'
#' @export
#'
#' @examples
#' # Pull in PIQL data from AWS and get some course data.
#' PIQLdata <- pullPQLdata()
#'
#' # Get answerkey
#' answerKey <- PIQLdata$answerkey
#'
#' # Pull course data and split into alpha and numerical variables
#' temp.data <- piql.data.select(PIQLdata, course = 1)
#' data.alpha <- temp.data$data.alpha
#' data.num <- temp.data$data.num
#'
#' # Check number of student removed. Should be less than 10%.
#' temp.data$nS.details
piql.data.select <- function(pulled.PIQL.data, courses = 1, numBlanks.allowed = 0) {
  PIQL.data <- pulled.PIQL.data$courses
  answerKey <- pulled.PIQL.data$answerkey
  courseList <- pulled.PIQL.data$courseList
  for (cc in 1:length(courses)) {
    if (courses[cc] > 100) {
      if (sum(courses[cc] == courseList) == 0 )  {
        warning("Course not in course list. Check course numbers.")
        stop()}
      courses[cc] = match(courses[cc], courseList) }
  }
  # Open retun varable so it can be appended in the for loop.
  thing.return <- list()
  thing.names <- c()
  ##########################################################################
  # Select course you would like to do an EFA on.
  ### This is currently built specifically for the PIQL. To update to a
  ### different set of data you can either:
  #### 1. update the AWS retrieval code above, or
  #### 2. input your data and answer key directly into
  ####     "data.alpha" and "answers" below.
  ############################################################################
  #      121, 122, 123, 141, 142, 143, 224, 225, 226, 321, 322, 323, 325
  # ii = 1,   2,   3,   4,   5,   6,   7,   8,   9,   10,  11,  12,  13
  for (cc in 1:length(courses) ) {
    course <- courses[cc]
    working.data <- PIQL.data[[course]]
    initial.nS <- nrow(working.data)
    # remove blanks
    working.data.noBlanks <- working.data[working.data$blanks <= numBlanks.allowed,]
    final.nS <- nrow(working.data.noBlanks)
    # Extract question information and grade questions.
    ## Currently this is done in a dichotomous fashion and does not
    ## handle MCMR items well
    data.alpha <- working.data.noBlanks[,substr(colnames(working.data.noBlanks),1,1) == "Q"]
    answers <- answerKey
    data.num <- array(NA, dim = c(nrow(data.alpha), ncol(data.alpha)))
    for (ii in 1:nrow(data.num)) {
      data.num[ii,] <- as.numeric(data.alpha[ii,] == answers)
    }
    # Give names to questions
    colnames(data.alpha) <- paste0("Q",c(1:ncol(data.alpha)))
    colnames(data.num) <- paste0("Q",c(1:ncol(data.num)))
    # Number of student details.
    nS.details <- data.frame(nS.initial = initial.nS,
                             nS.final = final.nS,
                             nS.lost.perc = (initial.nS-final.nS)/initial.nS)
    # Format return
    thing.return[[3*cc - 2]] <- data.alpha
    thing.return[[3*cc - 1]] <- data.num
    thing.return[[3*cc - 0]] <- nS.details
    thing.names <- c(thing.names,
                     noquote(paste0("data.alpha.", courseList[course])),
                     noquote(paste0("data.num.", courseList[course])),
                     noquote(paste0("nS.details.", courseList[course])))
  }
  names(thing.return) <- thing.names
  return(thing.return)
}



