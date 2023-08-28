#' Select specific course from the pulled PIQL data.
#'
#' @description Selects a specific course from the pulled PIQL data and separates
#' the correct MCMR options marked 1 if they were selected and 0 if they were not.
#'
#' @param selected.data Output from a data selection function (like piql.data.select)
#'
#' @param selected.questions A list of questions for each pulled course. Each course needs its own list
#' of questions. (NOT IMPLEMENTED YET)
#'
#' @return A list of course data filtered to include only the selected questions.
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
#' # Using Sample data in simplyPsycho
#' temp.data <- piql.data.select(simplySampleData, courses = c(1,2))
#' temp.data <- select.questions(temp.data)

select.questions <- function(selected.data, selected.questions = NULL, sort.selected.questions = FALSE) {
  # Find course numbers for input data
  return.data <- selected.data
  temp.names <- names(selected.data)[-c(length(names(selected.data))-1,length(names(selected.data)))]
  temp.data.nums <- ( (0:(length(temp.names)/3-1)) *3 ) + 1
  courseNames <- stringr::str_sub(temp.names[temp.data.nums], 12)

  for (cc in 1:length(courseNames)) {
    data.alpha.cc <- selected.data[[temp.data.nums[cc]]]
    data.numeric.cc <- selected.data[[temp.data.nums[cc]+1]]
    if ( is.null(selected.questions) == TRUE ) {
      # ----------------------------------- #
      # Prompt for questions
      selected.questions.cc <- c()
      flag.selection <- FALSE
      list.questions <- colnames(data.alpha.cc)
      while (flag.selection == FALSE) {
        cat("Which items would you like to include for ",  courseNames[cc],"?\n", sep = "")
        prompt.questions <- unlist(utils::select.list(as.list(c(list.questions, "Done"))))
        if (prompt.questions == "Done") {
          flag.selection = TRUE
        } else {
          selected.questions.cc <- c(selected.questions.cc,prompt.questions)
          list.questions <- list.questions[-match( prompt.questions,list.questions)]
        }
      }
    } else {
      if (length(temp.data.nums) != length(selected.questions)) {stop("Need one list of questions per course in the selected data.") }
      selected.questions
    }
    # ----------------------------------- #
    # Update current data
    if (sort.selected.questions == FALSE) {
      return.data[[temp.data.nums[cc]]] <- data.alpha.cc[,match(selected.questions.cc, colnames(data.alpha.cc))]
      return.data[[temp.data.nums[cc]+1]] <- data.numeric.cc[,match(selected.questions.cc, colnames(data.numeric.cc))]
    }
    if (sort.selected.questions == TRUE) {
      return.data[[temp.data.nums[cc]]] <- data.alpha.cc[,sort(match(selected.questions.cc, colnames(data.alpha.cc)))]
      return.data[[temp.data.nums[cc]+1]] <- data.numeric.cc[,sort(match(selected.questions.cc, colnames(data.numeric.cc)))]
    }
  }

  return(return.data)
}


