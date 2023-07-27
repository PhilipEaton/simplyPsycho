#' Select specific course from the pulled data - Walkthrough.
#'
#' @description Guides the user through selecting the correct data from the
#' pulled data from either AWS or from their own computer.
#'
#' @param pulled.data Output from pullPQLdata or pullData or import.from.computer
#'
#' @return A list of course data:
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
#' # Select data via walkthrough using data pulled
#' # from AWS or personal computer.
#' data.select.Walkthrough(pulled.data)
data.select.Walkthrough <- function(pulled.data) {
  data <- pulled.data$courses
  nQ <- sum(substr(colnames(data[[1]]),1,1) == "Q")
  answerKey <- pulled.data$answerkey
  courseList <- pulled.data$courseList
  # ----------------------------------- #
  # Prompt for which courses user wants
  selected.courses <- c()
  courses <- c()
  current.course.list <- courseList
  flag.course.select = FALSE
  while (flag.course.select == FALSE) {
    cat("Which course would you like to include in your analysis?")
    prompt.course.select <- unlist(utils::select.list(as.list(current.course.list)))
    selected.courses <- c(selected.courses, prompt.course.select)
    current.course.list <- current.course.list[-match(prompt.course.select,current.course.list)]
    cat("\n\nAdd another course?")
    prompt.course.more <- unlist(utils::select.list(as.list(c("Yes","No"))))
    if (prompt.course.more == "No") {flag.course.select = TRUE}
  }
  # ----------------------------------- #
  # Load in selected courses
  for (cc in 1:length(selected.courses)) {
      if (sum(selected.courses[cc] == courseList) == 0 )  {
        warning("Course not in course list. Check course numbers.")
        stop()}
      courses[cc] = as.numeric(noquote(match(selected.courses[cc], courseList)))
  }
  # ----------------------------------- #
  # Prompt for numBlanks
  flag.numBlanks <- FALSE
  while (flag.numBlanks == FALSE) {
    cat("How many blank entries (maximum) will you allow for a single student?\n")
    prompt.numBlanks <- readline(prompt = cat("Must be an integer larger than or equal to 0 and less than or equal to ", nQ, ".", sep = ""))
    if (is.na(suppressWarnings(as.numeric(prompt.numBlanks)))) {cat(crayon::cyan("Not a viable entry. Try again."))
    } else if ( (as.numeric(prompt.numBlanks) >= 0) && (as.numeric(prompt.numBlanks) <= nQ) && (round(as.numeric(prompt.numBlanks),0) == as.numeric(prompt.numBlanks)) ) {
      flag.numBlanks = TRUE
      numBlanks.allowed <- as.numeric(prompt.numBlanks)
    } else {cat(crayon::cyan("Not a viable entry. Try again.\n"))}
  }
  # ----------------------------------- #
  # Prompt for time constraints
  cat("Do you want to set time constraints?\n")
  prompt.constrainTime <- unlist(utils::select.list(as.list(c("Yes", "No"))))
  if (prompt.constrainTime == "No") {minTime.allowed = NA}
  if (prompt.constrainTime == "Yes") {
    prompt.constrainTime = TRUE
    flag.timeMin <- FALSE
    flag.timeMax <- FALSE
    while (flag.timeMin == FALSE) {
      cat("What *minimum* amount of working time would you like to set?\n")
      prompt.minTime <- readline(prompt = cat("Must be larger than or equal to 0."))
      if (is.na(suppressWarnings(as.numeric(prompt.minTime)))) {cat(crayon::cyan("Not a viable entry. Try again."))
      } else if ( (as.numeric(prompt.minTime) >= 0) ) {
        flag.timeMin = TRUE
        minTime.allowed <- as.numeric(prompt.minTime)
      } else {cat(crayon::cyan("Not a viable entry. Try again.\n"))}
    }
    while (flag.timeMax == FALSE) {
      cat("What *maximum* amount of working time would you like to set?\n")
      prompt.maxTime <- readline(prompt = cat("Must be larger than ", minTime.allowed, ".", sep = ""))
      if (is.na(suppressWarnings(as.numeric(prompt.maxTime)))) {cat(crayon::cyan("Not a viable entry. Try again."))
      } else if ( (as.numeric(prompt.maxTime) > minTime.allowed) ) {
        flag.timeMax = TRUE
        maxTime.allowed <- as.numeric(prompt.maxTime)
      } else {cat(crayon::cyan("Not a viable entry. Try again.\n"))}
    }
  }
  # ----------------------------------- #
  # Prompt for Matched
  if (length(selected.courses) > 1) {
    cat("Would you like this to be a matched set?")
    prompt.matched <- unlist(utils::select.list(as.list(c("Yes","No"))))
    if (prompt.matched == "Yes") {Matched = TRUE} else {Matched = FALSE}
    if (Matched == TRUE) {
      # Check for multiple courses. Quit if not.
      if (length(courses)==1) {stop("Need more than one course to match data.")
      } else {
        data[[courses[1]]] <- data[[courses[1]]][data[[courses[1]]]$blanks<=numBlanks.allowed,]
        if ( is.na(minTime.allowed) == FALSE ) {
          data[[courses[1]]] <- data[[courses[1]]][as.numeric(data[[courses[1]]]$Time) >= minTime.allowed,]
          data[[courses[1]]] <- data[[courses[1]]][as.numeric(data[[courses[1]]]$Time) <= maxTime.allowed,]
        }
        temp.student.list <- data[[courses[1]]]$Student.Code
        for (ss in 2:length(courses)) {
          data[[courses[ss]]] <- data[[courses[ss]]][data[[courses[ss]]]$blanks<=numBlanks.allowed,]
          if ( is.na(minTime.allowed) == FALSE ) {
            data[[courses[ss]]] <- data[[courses[ss]]][as.numeric(data[[courses[ss]]]$Time) >= minTime.allowed,]
            data[[courses[ss]]] <- data[[courses[ss]]][as.numeric(data[[courses[ss]]]$Time) <= maxTime.allowed,]
          }
          temp.student.list <- temp.student.list[na.omit(match(data[[courses[ss]]]$Student.Code,temp.student.list))]
        }
      }
      if (length(temp.student.list)==0) {stop("No continuous matches across all courses.")}
      for (ss in 1:length(courses)) {
        data[[courses[ss]]] <- data[[courses[ss]]][na.omit(match(temp.student.list,data[[courses[ss]]]$Student.Code)),]
      }
    }
  } else {Matched = FALSE}
  # ----------------------------------- #
  # Prompt for MCMR.items
  MCMR.items <- c()
  flag.MCMR.items <- FALSE
  current.SR.items <- c(1:nQ)
  current.MCMR.items <- c()
  while (flag.MCMR.items == FALSE) {
    cat("Which items are Multiple Choice Multiple Response (MCMR)?\n")
    prompt.MCMR <- unlist(utils::select.list(as.list(c(current.SR.items, "Done"))))
    if (prompt.MCMR == "Done") {
      flag.MCMR.items = TRUE
    } else {
      current.MCMR.items <- c(current.MCMR.items,prompt.MCMR)
      current.SR.items <- current.SR.items[-match( prompt.MCMR,current.SR.items)]
    }
  }
  if ( is.null(MCMR.items) == FALSE ) {
    current.MCMR.items <- as.numeric(current.MCMR.items)
  }
  current.SR.items <- as.numeric(current.SR.items)
  # ----------------------------------- #
  # Prompt for MCMR.grading
  if (!is.null(current.MCMR.items)) {
    cat("How would you like to grade the MCMR items?\n")
    prompt.MCMR.grading <- unlist(utils::select.list(as.list(c("Dichotomous", "Selected", "FourScale"))))
  } else {
    prompt.MCMR.grading <- "None"
  }

  # ----------------------------------- #
  # Activate grading!
  MCMR.items <- as.numeric(current.MCMR.items)
  MCMR.grading <- prompt.MCMR.grading
  # Open return variable so it can be appended in the for loop.
  thing.return <- list()
  thing.names <- c()
  for (cc in 1:length(courses) ) {
    course <- courses[cc]
    working.data <- data[[course]]
    initial.nS.before.time.cuts <- nrow(working.data)
    # Remove time blocks
    if ( is.na(minTime.allowed) == FALSE) {
      working.data <- working.data[as.numeric(working.data$Time) >= minTime.allowed,]
      working.data <- working.data[as.numeric(working.data$Time) <= maxTime.allowed,]
    }
    # remove blanks
    initial.nS <- nrow(working.data)
    working.data.noBlanks <- working.data[working.data$blanks <= numBlanks.allowed,]
    final.nS <- nrow(working.data.noBlanks)
    # Extract question information and grade questions.
    ## Currently this is done in a dichotomous fashion and does not
    ## handle MCMR items well
    data.alpha <- working.data.noBlanks[,substr(colnames(working.data.noBlanks),1,1) == "Q"]
    data.alpha <- noquote(apply(data.alpha, c(1,2), toupper))
    answers <- apply(answerKey, 2, toupper)


    ##########################################
    # Extract question information and grade single response questions.
    data.num.SR <- array(NA, dim = c(nrow(data.alpha), length(current.SR.items))) # Hard coded to the PIQLs number of SR questions
    colnames(data.num.SR) <- colnames(data.alpha)[current.SR.items]
    for (ii in 1:nrow(data.num.SR)) {
      data.num.SR[ii,] <- as.numeric(data.alpha[ii,current.SR.items] == answers[current.SR.items])
    }
    ##########################################

    #########################################
    # MCMR Data
    if (length(MCMR.items)>0) {
      mcmr.data.alph <- data.alpha[,MCMR.items]
      mcmr.answers <- unlist((answers[MCMR.items]))
      nMCMR <- length(MCMR.items)
      if (MCMR.grading == "Selected") {
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
            if (chars.stud.in.ans == 0) {data.num.MCMR[ss,qq] = 0
            } else if (chars.ans == chars.stud) {data.num.MCMR[ss,qq] = 3
            } else if (chars.stud == chars.stud.in.ans) {data.num.MCMR[ss,qq] = 2
            } else {data.num.MCMR[ss,qq] = 1}
          }
        }
        colnames(data.num.MCMR) <- paste0("Q",MCMR.items)
      } else {
        data.num.MCMR <- array(NA, dim = c(nrow(data.alpha), nMCMR))
        for (ss in 1:final.nS) {
          data.num.MCMR[ss,] <- as.numeric(mcmr.data.alph[ss,] == mcmr.answers)
        }
        colnames(data.num.MCMR) <- paste0("Q",MCMR.items)
      }
    }
    # Combine SR and MCMR data
    data.num <- cbind(data.num.SR,data.num.MCMR)
    data.num[is.na(data.num)] = 0
    # Give names to questions
    colnames(data.alpha) <- paste0("Q",c(1:ncol(data.alpha)))
    # Number of student details.
    if ( is.na(minTime.allowed) == FALSE) {
      nS.details <- data.frame(nS.starting = initial.nS.before.time.cuts,
                               nS.after.time.cuts = initial.nS,
                               nS.after.blanks.cuts = final.nS,
                               nS.lost.to.time.perc = (initial.nS.before.time.cuts-initial.nS)/initial.nS.before.time.cuts,
                               nS.lost.to.blanks.perc = (initial.nS-final.nS)/initial.nS)
    }
    if ( is.na(minTime.allowed) == TRUE) {
      nS.details <- data.frame(nS.starting = initial.nS,
                               nS.after.blanks.cuts = final.nS,
                               nS.lost.to.blanks.perc = (initial.nS-final.nS)/initial.nS)
    }


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
  thing.return$MCMRitems <- MCMR.items
  thing.return$Matched <- Matched
  return(thing.return)
}

