#' Pull  data from AWS
#'
#' @description Walks the user through pulling available data from AWS.
#'
#' @param accessID AWS access ID.
#'
#' @param accessSecret AWS secret access ID.
#'
#' @param data.directory (Default = "piqlgerqndata") Name of the directory in AWS
#' the user is extracting data from.
#'
#' @param name.assessment Name of the assessment who data is to be extracted. Default
#' option will result in a walk through helping the user select the correct data.
#'
#' @param region (Default = "us-west-2") AWS region ID for database.
#'
#' @return A list of course data stored in AWS for the requested assessment.
#'
#' @export
#'
#' @examples
#' # Pull in data from AWS and get some course data.
#' pulledData <- pulldata()
#' pulledData$courses # Course data
#' pulledData$answerkey # answer key
pulldata <- function(accessID, accessSecret, data.directory = "piqlgerqndata", name.assessment = NULL, region = "us-west-2") {
  ###############################################################################
  # Pull in data from AWS
  ###############################################################################
  Sys.setenv(
    "AWS_ACCESS_KEY_ID" = accessID,
    "AWS_SECRET_ACCESS_KEY" = accessSecret,
    "AWS_DEFAULT_REGION" = region
  )

  dataDirectory <- data.directory
  dataContents <- aws.s3::get_bucket_df(bucket = dataDirectory, region = region)
  dataFileNames <- as.vector(dataContents$Key)
  ### ------------------------------------------
  ### Assessment names current in data
  names.assessments <- c()
  for (ii in 1:length(dataFileNames)) {
    if (suppressWarnings(!is.na(as.numeric(substr(dataFileNames[ii],1,3)))) == TRUE) {
      names.assessments <- c(names.assessments, unlist(strsplit(dataFileNames[ii],"_"))[3])
    }
  }
  names.assessments <- names.assessments[!duplicated(names.assessments)]
  ### ------------------------------------------
  ### Identify which keys are present and store those names
  names.keys <- c()
  for (ii in 1:length(dataFileNames)) {
    flag.key <- !identical(as.integer(grep("key",dataFileNames[ii])), integer(0))
    if (flag.key == TRUE) {
      names.keys <- c(names.keys, dataFileNames[ii])
    }
  }
  names.keys <- c(names.keys, "Set Later")

  ### ------------------------------------------
  ### Get Assessment name if not specified
  if ( is.null(name.assessment) == TRUE) {
    flag.name <- FALSE
    while (flag.name == FALSE) {
      cat("\n\n\n")
      name.assessment <- readline(prompt = cat("Which assessment would you like data from -- ", paste(names.assessments, collapse = " or "), ": ", sep = ""))
      if (name.assessment %in% names.assessments) {
        flag.name = TRUE
        } else {print("Not a viable assessment name. Try again.")}
      }
  } else if (name.assessment %in% names.assessments == FALSE) {
    cat("Not a viable assessment name. Try again or check that data files names are formatted correctly.
File names in AWS should be formatted as ###_###_NAME \n")
    stop("Given assessment name does not exist in AWS pool.")
  }

  ### ------------------------------------------
  ### Get file names for requested assessment
  dataFileNames <- dataFileNames[grep(name.assessment, as.list(dataFileNames))]
  if (name.assessment == "PIQL") {
    check.length <- 22
  } else if (name.assessment == "GERQN") {check.length <- 23 }
  # Check names
  temp.toRemove <- c()
  for (ii in 1:length(dataFileNames)) {
    if (nchar(dataFileNames[ii]) != check.length) {
      flag.keep <- FALSE
      while (flag.keep == FALSE) {
        cat("\n\n\n")
        flag.keep <- readline(prompt = cat("Keep ", dataFileNames[ii]," in data? 1 = Yes ; 2 = No: ", sep = ""))
        if (flag.keep == "1") {flag.keep = TRUE
        } else if (flag.keep == "2") {
          flag.keep = TRUE
          temp.toRemove <- c(temp.toRemove,ii)
        } else {
            flag.keep = FALSE
            print("Not a viable response. Try again.")}
        }
      }
  }
  # IF there are files to remove, then remove them.
  if (length(temp.toRemove) > 0) {dataFileNames <- dataFileNames[-temp.toRemove]}

  ### ------------------------------------------
  ### Get the term and course numbers in current data
  term <- c()
  courses <- c()
  for (ii in 1:length(dataFileNames)) {
    term <- c(term,substr(dataFileNames[ii],1,3))
    courses <- c(courses,substr(dataFileNames[ii],5,7))
  }
  term <- term[!duplicated(term)]
  courses <- courses[!duplicated(courses)]

  ## -------------------------------------------------------
  ## Get answer key
  flag.key <- FALSE
  while (flag.key == FALSE) {
    cat("\n\n\n")
    cat("Select from the following list \n ")
    name.key <- unlist(utils::select.list(as.list(names.keys)))
    #name.key <- readline(prompt = cat("Enter the file name of the key you would like to use -- \n\n",
    #paste(names.keys, collapse = " or "), "\n\nOR would like to input your key manually? If so, enter 1.", sep = ""))
    if (name.key %in% names.keys) {
      flag.key = TRUE
    } else {cat("Not a viable entry. Try again.")}
  }
  ## -------------------------------------------------------
  ## Se up answer key
  if (name.key == "Set Later") {answerkey <- c("Set Later")
  } else{
  answers <- suppressWarnings(aws.s3::s3read_using(FUN = read.csv, bucket = dataDirectory, object = name.key))
  if(length(answers) == 1){
    answerkey <- read_csv(answers, na = c("N", "", " ", "n", "NA", "999"))
    answerkey[] <- lapply(answerkey, gsub, pattern=',', replacement='') ## Get rid of commas and spaces in the answerkey
    answerkey[] <- lapply(answerkey, gsub, pattern=' ', replacement='')
  } else{
    answerkey <- as.data.frame(matrix(answers, nrow = 1))
  }
  numQs <- length(answerkey)
  questionNumbers <- c(1:length(answerkey))
  colnames(answerkey) <- c(paste0("Q",questionNumbers))
  }

  ## -------------------------------------------------------
  ##  Set up the datafile frame for imported data and data collected by course
  dataFiles <- as.data.frame(matrix(data.frame(), nrow = length(term), ncol = length(courses)))
  dataFilesByCourse <- as.data.frame(matrix(data.frame(),nrow=length(courses),ncol=1))

  ## pull the data, clean it, and score it
  for (ii in 1:length(dataFileNames)) {
    print(dataFileNames[ii])
    df <- match(unlist(strsplit(dataFileNames[ii],"_"))[1], term)
    c <- match(unlist(strsplit(dataFileNames[ii],"_"))[2], courses)
    # Put data into dataFiles
    dataFiles[[df, c]] <- aws.s3::s3read_using(FUN = read.csv, bucket = dataDirectory, object = dataFileNames[ii], na.strings = c("N","", " ", "n", "NA", "999", "777"))
    colnames(dataFiles[[df, c]])[substr(colnames(dataFiles[[df, c]]),1,1) == "Q"] <- paste0("Q",c(1:sum(substr(colnames(dataFiles[[df, c]]),1,1) == "Q")))
    dataFiles[[df,c]] <- dplyr::mutate(dataFiles[[df,c]], t = term[df])
    dataFiles[[df, c]][] <- lapply(dataFiles[[df, c]], gsub, pattern=',', replacement='') ## Remove any commas in the responses.
    dataFiles[[df, c]][] <- lapply(dataFiles[[df, c]], gsub, pattern=' ', replacement='') ## Remove any spaces in the responses.
    dataFiles[[df, c]] <- dplyr::mutate(dataFiles[[df, c]], blanks = 0) ## Add columns to be able to determine each student's total score and the number of questions left blank.
    qCols <- c(substr(colnames(dataFiles[[df,c]]),1,1) == "Q")
    for(i in 1:nrow(dataFiles[[df,c]])){
      dataFiles[[df,c]]$blanks[i] <- sum(is.na(dataFiles[[df,c]][i,qCols]))
    }
  }

  ## -------------------------------------------------------
  ## ---------- Combine Data Across Terms ------------------
  ## -------------------------------------------------------

  ## make an empty data set to get the dataFilesByCourse started
  # Set to be the same as the col names in the first set of data
  for(c in 1:length(courses)){
    colNamesByCourse = colnames(dataFiles[[1, c]])
    emptyDataFrame = data.frame(matrix(nrow = 0, ncol = length(colNamesByCourse)))
    colnames(emptyDataFrame) = colNamesByCourse
    dataFilesByCourse[[c,1]] <- emptyDataFrame
  }

  ## go through each course and term and find where there is data, then append it to the dataFrameByCourse
  ## then rename the score column to score[course].
  for(c in 1:length(courses)){
    for(i in 1:length(term)){
      if(!is.null(dataFiles[[i,c]])){
        dataFilesByCourse[[c,1]] <- rbind(dataFilesByCourse[[c,1]],dataFiles[[i,c]]) ## add data to dataFilesByCourse across terms.
      } ## end of if/else to filter for dataFiles that have data
    } ## end of for loop through terms
  } ## end of for loop through courses

  thing.return <- list()
  thing.return$courses <- dataFilesByCourse$V1
  thing.return$answerkey <- answerkey
  thing.return$courseList <- courses
  thing.return$term <- term
  return(thing.return)
  ###############################################################################
  # End data retrieval here
  ###############################################################################
}
