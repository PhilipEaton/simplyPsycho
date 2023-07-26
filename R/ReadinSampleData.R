simplySampleData <- list()
courses <- list()
course1 <- read.csv("SampleData.csv")[1:4000,-1]
course2 <- read.csv("SampleData.csv")[-c(1:4000),-1]
courses[[1]] <- course1
courses[[2]] <- course2
simplySampleData$courses <- courses
simplySampleData$answerkey <- t(read.csv("SampleKey.csv"))[2,]
simplySampleData$courseList <- c(111, 222)
simplySampleData$term <- c(000)
simplySampleData
rm(courses)
