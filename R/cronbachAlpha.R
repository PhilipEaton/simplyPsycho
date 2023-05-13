#' CTT Cronbach's Alpha:
#'
#' @description Calculate Cronbach's Alpha for an assessment from sample data..
#'
#' @param data An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
#' where nS is the number of students in the sample and nQ is the number of questions.
#'
#' @return Cronbach's Alpha value for the given graded data.
#'
#' @export
#'
#' @examples
#' # Pull in PIQL data from AWS and get some course data.
#' PIQLdata <- pullPQLdata()
#' temp.piql.data <- PIQLdata$courses
#' data.num <- temp.data$data.num
#'
#' # Cronbach's Alpha
#' cronbachAlpha(data.num)
cronbachAlpha <- function(data){
  nQ <- ncol(data) # Get number of questions
  item.var <- var(data) # Get variance matrix
  item.var.diag <- diag(item.var) # Save diagonal of variance
  diag(item.var) = 0 # Remove diagonal to get only covariances
  # Definition of Cronbach's Alpha
    return( (nQ / (nQ - 1)) * (1 - (sum(item.var.diag) / var(rowSums(data))) ) )
  }
