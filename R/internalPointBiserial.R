#' CTT Point-biserial:
#'
#' @description Calculates the point-biserial correlation between the total score
#' (with focus item removed) and the focus item. This gives a measure if the items
#' on the assessment are all measuring a single similar latent trait. If the point-biserial
#' is poor, then this could be an indication that the assessment is multidimensional.
#'
#'
#' @param data An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
#' where nS is the number of students in the sample and nQ is the number of questions.
#'
#' @return Point-biserial correlation between the total score (with focus item removed)
#' and the focus item. For all items on the assessment
#'
#' @export
#'
#' @examples
#' # Pull in PIQL data from AWS and get some course data.
#' PIQLdata <- pullPIQLdata()
#' temp.piql.data <- PIQLdata$courses
#' data.num <- temp.data$data.num
#'
#' # Point-biserial
#' pointBiserial(data.num)
internal.pointBiserial <- function(data){
  nQ <- ncol(data) # Get number of questions
  totalScores <- rowSums(data)
  rpb <- t(as.data.frame(numeric(nQ)))
  rownames(rpb) <- "point-biserial"
  for (ii in 1:nQ) {
    st.mn <- mean(totalScores)
    # Get mean total score and number of students who got item correct.
    M1 <- mean(totalScores[data[,ii] == 1])
    n1 <- length(totalScores[data[,ii] == 1])
    # same as above, but for incorrect.
    M0 <- mean(totalScores[data[,ii] == 0])
    n0 <- length(totalScores[data[,ii] == 0])
    # Total number of students
    n  <- n1 + n0
    # Sample standard deviation
    s <- sqrt( (1/(n-1)) * sum((totalScores - st.mn)^2) )
    # Calculate point-biserial
    rpb[ii] <- ((M1-M0)/(s)) * sqrt( ((n1*n0)/(n*(n-1))) )
  }
  # Formate return
  # Format return
  thing.return <- round(rpb, 3)
  if ( is.null(colnames(data)) )  {
    colnames(thing.return) <- paste0("Q",c(1:nQ))
  } else {
    colnames(thing.return) <- colnames(data)
  }
  return(thing.return)
}
