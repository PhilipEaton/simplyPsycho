#' Pull PIQL data from AWS
#'
#' @description Pulls PIQL data from AWS
#'
#' @return A list of course data for the PIQL stored in AWS.
#'
#'      $courses course data as a list.
#'
#'     $answerkey answer key stored in AWS.
#' @export
#'
#' @examples
#' # Pull in PIQL data from AWS and get some course data.
#' PIQLdata <- pullPQLdata()
#' PIQLdata$courses # Course data
#' PIQLdata$answerkey # answer key

pullPQLdata <- function() {
###############################################################################
# Pull in data from AWS
###############################################################################
Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "AKIA3HK7TAAACSRSO5PK",
  "AWS_SECRET_ACCESS_KEY" = "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",
  "AWS_DEFAULT_REGION" = "us-west-2"
)

dataDirectory <- "piqlgerqndata"
dataContents <- get_bucket_df(bucket = dataDirectory, region = "us-west-2")

### ------------------------------------------
### -------------- EDIT HERE!! ---------------
### ------------------------------------------
term <- c(204,211,212,214,221,222,224)
courses <- c(121,122,123,141,142,143,224,225,226,321,322,323,325)
### -------------- STOP EDITING!!!! ----------

## -------------------------------------------------------
## ---------- Get the data and score it ------------------
## -------------------------------------------------------
## set up the answer key
answers <- s3read_using(FUN = read.csv, bucket = dataDirectory, object = "piqlanswerkey.csv")
if(length(answers) == 1){
  answerkey <- read_csv(answers, na = c("N", "", " ", "n", "NA", "999"))
  answerkey[] <- lapply(answerkey, gsub, pattern=',', replacement='') ## Get rid of commas and spaces in the answerkey
  answerkey[] <- lapply(answerkey, gsub, pattern=' ', replacement='')
} else{
  answerkey <- as_tibble(matrix(answers, nrow = 1))
}
questionNumbers <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20) ## handwritten because I'm lazy and the test is static.
qCols <- c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)
numQs <- length(questionNumbers) ## Determine the number of questions from the column names
colnames(answerkey) <- c(paste0("Q",questionNumbers))

## make the following variables for each course/term: dataFileNames, score
for(i in 1:length(courses)){
  assign(paste0("dataFileNames",courses[i]),paste0(term, "_", courses[i],"_PIQL_PEGID.csv"))
}
## set up the datafile frame for imported data and data collected by course
dataFiles <- as.data.frame(matrix(data.frame(), nrow = length(term), ncol = length(courses)))
dataFilesByCourse <- as.data.frame(matrix(data.frame(),nrow=length(courses),ncol=1))

## pull the data, clean it, and score it
for(c in 1:length(courses)){
  for(df in 1:length(term)){
    if(sum(str_detect(dataContents$Key, paste0(term[df],"_",courses[c],"_PIQL_PEGID.csv")))>0){
      print(paste0(term[df],"_",courses[c],"_PIQL_PEGID.csv yes!"))
      dataFiles[[df, c]] <- s3read_using(FUN = read.csv, bucket = dataDirectory, object = paste0(term[df],"_",courses[c],"_PIQL_PEGID.csv"), na.strings = c("N","", " ", "n", "NA", "999", "777"))
      dataFiles[[df,c]] <- mutate(dataFiles[[df,c]], t = term[df])
      dataFiles[[df, c]][] <- lapply(dataFiles[[df, c]], gsub, pattern=',', replacement='') ## Remove any commas in the responses.
      dataFiles[[df, c]][] <- lapply(dataFiles[[df, c]], gsub, pattern=' ', replacement='') ## Remove any spaces in the responses.
      dataFiles[[df, c]] <- mutate(dataFiles[[df, c]], score = 0, blanks = 0) ## Add columns to be able to determine each student's total score and the number of questions left blank.
      for(i in 1:nrow(dataFiles[[df,c]])){
        dataFiles[[df,c]]$score[i] <- length(which(dataFiles[[df,c]][i,qCols] == answerkey))
        dataFiles[[df,c]]$blanks[i] <- length(which(is.na(dataFiles[[df,c]][i,qCols])))
      }
      dataFiles[[df,c]] <- mutate(dataFiles[[df,c]], percent = score/numQs * 100)
    }
  }
}

## -------------------------------------------------------
## ---------- Combine Data Across Terms ------------------
## -------------------------------------------------------

## make an empty data set to get the dataFilesByCourse started
colNamesByCourse = c("Student.Code", "Time","Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17","Q18","Q19","Q20","t","score","blanks","percent")
emptyDataFrame = data.frame(matrix(nrow = 0, ncol = length(colNamesByCourse)))
colnames(emptyDataFrame) = colNamesByCourse
for(c in 1:length(courses)){
  dataFilesByCourse[[c,1]] <- emptyDataFrame
}

## go through each course and term and find where there is data, then append it to the dataFrameByCourse
## then rename the score column to score[course].
for(c in 1:length(courses)){
  for(i in 1:length(term)){
    if(is.null(dataFiles[[i,c]])){
    }else{
      dataFilesByCourse[[c,1]] <- rbind(dataFilesByCourse[[c,1]],dataFiles[[i,c]]) ## add data to dataFilesByCourse across terms.
    } ## end of if/else to filter for dataFiles that have data
  } ## end of for loop through terms
  colnames(dataFilesByCourse[[c,1]])[colnames(dataFilesByCourse[[c,1]]) == "score"] = paste0("score",courses[c]) ## rename the score column to include course tag
} ## end of for loop through courses

thing.return <- list()
thing.return$courses <- dataFilesByCourse$V1
thing.return$answerkey <- answerkey
return(thing.return)
###############################################################################
# End data retrieval here
###############################################################################
}
