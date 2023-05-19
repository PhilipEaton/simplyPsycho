#' Select specific course from the pulled PIQL data.
#'
#' @description Selects a specific course from the pulled PIQL data and separates
#' the correct MCMR options marked 1 if they were selected and 0 if they were not.
#'
#' @param pulled.PIQL.data Output from pullPQLdata
#'
#' @param course (Default = 1 = 121) Course number (1 = 121, 2 = 122, 3 = 123, etc.) or position in course list:
#'
#' Course: 121, 122, 123, 141, 142, 143, 224, 225, 226, 321, 322, 323, 325
#'
#' @param MCMR.grading (Default = "Dichotomous") If undefined, then MCMR items will be graded dichotomously.
#'
#' If "Selected", then selected = 1 and not selected = 0 for correct options on
#' the MCMR items will be performed.
#'
#' If "FourScale", then MCMR items will be graded o the 4 scale:
#' (0 = completely incorrect, 1 = correct and incorrect, 2 = some correct, 3 = completely correct)
#'
#' @param numBlanks.allowed (default = 0) Number of blanks allowed in the resulting data.
#'
#' @param Matched (Default = FALSE). If TRUE, only matching student IDs will be extracted
#' across all courses.
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
#' # Get PIQL data
#' PIQLdata <- pullPIQLdata()
#' temp.data <- piql.data.select(PIQLdata, MCMR.grading = "Selected", course = 1)
#' data.alpha <- temp.data$data.alpha
#' data.num <- temp.data$data.num
#' # Check number of student removed. Should be less than 10%.
#' temp.data$nS.details
#'
piql.data.select <- function(pulled.PIQL.data, MCMR.grading = "Dichotomous", courses = 1, numBlanks.allowed = 0, Matched = FALSE) {
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
  if (Matched == TRUE) {
    # Check for multiple courses. Quit if not.
    if (length(courses)==1) {stop("Need more than one course to match data.")
    } else {
      PIQL.data[[courses[1]]] <- PIQL.data[[courses[1]]][PIQL.data[[courses[1]]]$blanks<=numBlanks.allowed,]
      temp.student.list <- PIQL.data[[courses[1]]]$Student.Code
      for (ss in 2:length(courses)) {
        PIQL.data[[courses[ss]]] <- PIQL.data[[courses[ss]]][PIQL.data[[courses[ss]]]$blanks<=numBlanks.allowed,]
        temp.student.list <- temp.student.list[na.omit(match(PIQL.data[[courses[ss]]]$Student.Code,temp.student.list))]
      }
    }
    if (length(temp.student.list)==0) {stop("No continuous matches across all courses.")}
    for (ss in 1:length(courses)) {
      PIQL.data[[courses[ss]]] <- PIQL.data[[courses[ss]]][na.omit(match(temp.student.list,PIQL.data[[courses[ss]]]$Student.Code)),]
    }
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


    ##########################################
    # Extract question information and grade single response questions.
    data.num.SR <- array(NA, dim = c(nrow(data.alpha), 14)) # Hard coded to the PIQLs number of SR questions
    colnames(data.num.SR) <- paste0("Q",c(1:ncol(data.num.SR)))
    for (ii in 1:nrow(data.num.SR)) {
      data.num.SR[ii,] <- as.numeric(data.alpha[ii,1:14] == answers[1:14])
    }
    ##########################################

    #########################################
    # MCMR Data
    mcmr.data.alph <- data.alpha[,15:20]
    mcmr.answers <- unlist((answers[15:20]))
    nMCMR <- 6
    if (MCMR.grading == "Selected") {
      ##########################################
      # Extract question information and store correct response selection for
      ## MCMR items.
      #
      ## I tried to make this as general as I could. I know there are more efficient
      ## ways of doing this, but they would be less general. I want this code to be
      ## adaptable for application to the GERKIN and what not.

      # First build column names
      mcmr.names <- c()
      for (qq in 1:nMCMR) {
        cur.ans <- unlist(strsplit(mcmr.answers[qq], split = ""))
        for (oo in 1:length(cur.ans)) {
          mcmr.names <- c(mcmr.names, paste0("Q",qq+ncol(data.num.SR),".",cur.ans[oo]))
        }
      }
      data.num.MCMR <- array(NA, dim = c(nrow(data.alpha), length(mcmr.names)))
      colnames(data.num.MCMR) <- mcmr.names
      for (ss in 1:nrow(data.alpha)) {
        num.ans <- 0
        for (qq in 1:nMCMR) {
          cur.ans <- unlist(strsplit(mcmr.answers[qq], split = ""))
          for (oo in 1:length(cur.ans)) {
            data.num.MCMR[ss,(num.ans+oo)] <- as.numeric(grepl(cur.ans[oo], mcmr.data.alph[ss,qq]))
          }
          num.ans <- num.ans + length(cur.ans)
        }
      }
    } else if (MCMR.grading == "FourScale") {
      data.num.MCMR <- array(NA, dim = c(nrow(data.alpha), nMCMR))
      for (qq in 1:nMCMR) {
        chars.ans <- nchar(mcmr.answers[qq])
        for (ss in 1:final.nS) {
          chars.stud <- nchar(mcmr.data.alph[ss,qq])
          chars.stud.in.ans <- sum(!is.na(match(unlist(strsplit(mcmr.answers[qq], split = "")),unlist(strsplit(mcmr.data.alph[ss,qq], split = "")))))
          #if (sum(is.na(chars.stud.in.ans))>0) {chars.stud.in.ans <- 0}
          if (chars.stud.in.ans == 0) {data.num.MCMR[ss,qq] = 0
          } else if (chars.ans == chars.stud) {data.num.MCMR[ss,qq] = 3
          } else if (chars.stud == chars.stud.in.ans) {data.num.MCMR[ss,qq] = 2
          } else {data.num.MCMR[ss,qq] = 1}
        }
      }
      colnames(data.num.MCMR) <- paste0("Q",c(15:20))
    } else {
       data.num.MCMR <- array(NA, dim = c(nrow(data.alpha), nMCMR))
       for (ss in 1:final.nS) {
         data.num.MCMR[ss,] <- as.numeric(mcmr.data.alph[ss,] == mcmr.answers)
       }
      colnames(data.num.MCMR) <- paste0("Q",c(15:20))
    }
    # Combine SR and MCMR data
    data.num <- cbind(data.num.SR,data.num.MCMR)
    # Give names to questions
    colnames(data.alpha) <- paste0("Q",c(1:ncol(data.alpha)))
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
