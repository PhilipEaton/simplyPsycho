#' Import data from computer
#'
#' @description Select files from your computer to be used in siplyPsycho. The
#' function asks for descriptive information about the files (student data or
#' answer key). If student data, then it asks for the course it is from and for
#' the term. formatting is checked via build.psycho.datafile. See that function's
#' documentation for the formatting extected for student data.
#'
#'
#' @return Data to be used in simplyPsycho with correct formatting.
#'
#' @import readxl
#'
#' @export
#'
#' @examples
#' # Pull in from your computer
#' ## Data must be formatted correctly - see documentation.
#'
#' import.from.computer()
#'
#' # Follow the prompts.
#' # Data should be formatted in the same manner as simplySampleData01 available
#' # in the simplyPsycho Github repository (https://github.com/PhilipEaton/simplyPsycho).

import.from.computer <- function() {
  return.master <- list()
  answerKey = NA
  flag.moredata <- TRUE
  while (flag.moredata == TRUE) {
    # ---------------------- #
    #' Read in file from computer and pull the last letters to check file type.
    temp.filename <- file.choose()
    check.type <- substr(temp.filename, nchar(temp.filename)-2, nchar(temp.filename))
    if (check.type == "lsx") {
      num.sheets <- length(unlist(readxl::excel_sheets(temp.filename)))
      if (num.sheets > 1) {
        sheet.names <- readxl::excel_sheets(path = temp.filename)
        for (ss in 1:num.sheets) {
          temp.files <- as.data.frame(readxl::read_excel(temp.filename, sheet = ss))
          # ---------------------- #
          ## What kind of file is this?
          cat("\n\n\n")
          cat(paste0("What kind of data is on sheet ", sheet.names[ss], " ?", sep = ""))
          temp.type <- unlist(utils::select.list(as.list(c("Student Response Data", "Answer Key"))))
          if (temp.type == "Student Response Data") {
            prompt.course <- readline(prompt = cat("What course is this from (try to use the course number, if possible): "))
            prompt.term <- readline(prompt = cat("If from a single term, what term is this from? If not, put NA: "))
            if (prompt.term == "NA") {prompt.term = NA}
            # ------------------------ #
            # Add course to the return.
            return.master <- build.psycho.datafile(return.master,temp.files, new.course.name = prompt.course,
                                                   new.term = prompt.term, answerKey = answerKey)
            } else if (is.null(answerKey) == FALSE) { print("Answer key was already installed. No changes were made.\n\n\n")
            } else {
              if (dim(temp.files)[1] == 1) {
                answerKey <- temp.files[1,]
                colnames(answerKey) <- paste0("Q", c(1:length(answerKey)))
              } else {
                temp.files <- t(temp.files)
                answerKey <- temp.files[2,]
                colnames(answerKey) <- paste0("Q", c(1:length(answerKey)))
                }
              return.master <- build.psycho.datafile(return.master,NA, new.course.name = NA,
                                                     new.term = NA, answerKey = answerKey)}
          }
        } else if (num.sheets == 1) {
        temp.files <- as.data.frame(readxl::read_excel(temp.filename))
        # ---------------------- #
        ## What kind of file is this?
        cat("\n\n\n")
        cat("What kind of data is this?")
        temp.type <- unlist(utils::select.list(as.list(c("Student Response Data", "Answer Key"))))
        # ---------------------- #
        ## What kind of file is this?
        cat("What kind of data is this?")
        temp.type <- unlist(utils::select.list(as.list(c("Student Response Data", "Answer Key"))))
        if (temp.type == "Student Response Data") {
          prompt.course <- readline(prompt = cat("What course is this from (try to use the course number, if possible): "))
          prompt.term <- readline(prompt = cat("If from a single term, what term is this from? If not, put NA: "))
          # ------------------------ #
          # Add course to the return.
          return.master <- build.psycho.datafile(return.master,temp.files, new.course.name = prompt.course,
                                                 new.term = prompt.term, answerKey = answerKey)
        } else if (is.null(answerKey) == FALSE) { print("Answer key was already installed. No changes were made.\n\n\n")
        } else {
          if (dim(temp.files)[1] == 2) {
            answerKey <- temp.files[2,]
            colnames(answerKey) <- noquote(temp.files[1,])
          } else {
            temp.files <- t(temp.files)
            answerKey <- temp.files[2,]
            names(answerKey) <- noquote(temp.files[1,])
          }
          return.master <- build.psycho.datafile(return.master,NA, new.course.name = NA,
                                                 new.term = NA, answerKey = answerKey)}
      }
    } else if (check.type == "csv") {
      temp.files <- as.data.frame(read.csv(temp.filename))
      # ---------------------- #
      ## What kind of file is this?
      cat("\n\n\n")
      cat("What kind of data is this?")
      temp.type <- unlist(utils::select.list(as.list(c("Student Response Data", "Answer Key"))))
      # ---------------------- #
      ## What kind of file is this?
      cat("What kind of data is this?")
      temp.type <- unlist(utils::select.list(as.list(c("Student Response Data", "Answer Key"))))
      if (temp.type == "Student Response Data") {
        prompt.course <- readline(prompt = cat("What course is this from (try to use the course number, if possible): "))
        prompt.term <- readline(prompt = cat("If from a single term, what term is this from? If not, put NA: "))
        # ------------------------ #
        # Add course to the return.
        return.master <- build.psycho.datafile(return.master,temp.files, new.course.name = prompt.course,
                                               new.term = prompt.term, answerKey = answerKey)
      } else if (is.null(answerKey) == FALSE) { cat("Answer key was already installed. No changes were made.\n\n\n")
      } else {
        if (dim(temp.files)[1] == 2) {
          answerKey <- temp.files[2,]
          colnames(answerKey) <- noquote(temp.files[1,])
        } else {
          temp.files <- t(temp.files)
          answerKey <- temp.files[2,]
          names(answerKey) <- noquote(temp.files[1,])
        }
        return.master <- build.psycho.datafile(return.master,NA, new.course.name = NA,
                                               new.term = NA, answerKey = answerKey)}
    } else {
      stop("Filenames must end with be either .csv or .xlsx. File type must match the extension indicator.")
      }
    # ---------------------- #
    ## Prompt for more data
    cat("Would you like to add more files (student response data or answer key)?")
    prompt.moredata <- unlist(utils::select.list(as.list(c("Yes", "No"))))
    if (prompt.moredata == "No") {flag.moredata = FALSE}
  }
  # RETURN
  return(return.master)
}

