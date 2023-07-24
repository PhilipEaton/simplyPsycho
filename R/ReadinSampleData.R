simplySampleData <- list()
courses <- list()
courses[[1]] <- read.csv("SampleData.csv")[,-1]
simplySampleData$courses <- courses
simplySampleData$answerkey <- LETTERS[read.csv("SampleKey.csv")[,-1] + 1]
simplySampleData$courseList <- c(111)
simplySampleData$term <- c(000)
simplySampleData
rm(courses)

