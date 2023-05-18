#' Walkthrough of an Exploratory Factor Analysis
#'
#' @description Uses prompts to walks the user through an EFA of the given data.
#'
#' @param data Can be either
#'
#' 1) An nS by nQ matrix or data frame of a dichotomous graded (0 or 1) sample,
#' where nS is the number of students in the sample and nQ is the number of questions,
#'
#' 2) The output of piql.data.select with one or multiple courses selected.
#'
#' @return Walks user through an EFA of the data.
#'
#' @export
#'
#' @examples
#' # Pull in PIQL data from AWS and get some course data.
#' PIQLdata <- pullPIQLdata()
#' temp.piql.data <- PIQLdata$courses
#' data.num <- temp.data$data.num
#'
#' Begin walkthrough
#' efa.walkthrough(data.num)
efa.walkthrough <- function (data){
  if (typeof(data) == "list") {
    data <- data[[2]]
  }

  nS <- nrow(data)
  nQ <- ncol(data)
  # First select the type of correlation to be performed:
  ## Prompt for correlation type:
  flag.cor <- TRUE
  while (flag.cor == TRUE) {
    type.cor <- as.numeric(readline(prompt = "Which type of correlation would you like to use? 1 = Pearson r ; 2 = Polychoric correlation: "))
    if (type.cor == 1 || type.cor == 2) {
      flag.cor = FALSE
    } else {print("That is not a viable entry.")}
  }
  ## Calculate correlation:
  if (type.cor == 1) {
    cor.mat <- cor(data)
  } else if (type.cor == 2) {
    cor.mat <- psych::polychoric(data)$rho
  }

  # Now check for factorability:
  ## Prompt
  flag.factorability <- TRUE
  while (flag.factorability == TRUE) {
    check.factorability <- as.numeric(readline(prompt = "Would you like to check factorability of the data? 1 = Yes ; 2 = No: "))
    if (check.factorability == 1 || check.factorability == 2) {
      flag.factorability = FALSE
    } else {print("That is not a viable entry.")}
  }
  # Display results:
  if (check.factorability == 1) {
    thing1 <- EFAtools::BARTLETT(data)
    thing2 <- EFAtools::KMO(cor.mat)
    print(thing1)
    cat("\n")
    print(thing2)
    cat("\n")
  }

  # Ask to run parallel analysis
  ## Prompt
  flag.run.paran <- TRUE
  while (flag.run.paran == TRUE) {
    check.paran <- as.numeric(readline(prompt = "Would you like run parallel analysis to estimate the number of factors? 1 = Yes ; 2 = No: "))
    if (check.paran == 1 || check.paran == 2) {
      flag.run.paran = FALSE
    } else {print("That is not a viable entry.")}
  }
  ## Run if requested
  if (check.paran == 1) {
    #print(EFAtools::N_FACTORS(cor.mat, N = nS, method = "ULS", plot = FALSE))
    print(EFAtools::N_FACTORS(cor.mat, N = nS, criteria = c("PARALLEL", "EKC", "SMT"),
                              eigen_type_other = c("SMC", "PCA")))
  }

  # Ask for number of factors:
  ## Prompt
  flag.keepGoing <- TRUE
  while (flag.keepGoing == TRUE) {
    check.num.factors <- as.integer(readline(prompt = "Number of factors? Enter an integer that is larger than 1, or 0 to exit: "))
    if ( (check.num.factors > 1) && (round(check.num.factors)==check.num.factors) && (check.num.factors < nQ) ) {
      print(EFAtools::EFA(cor.mat, n_factors = check.num.factors, rotation = "oblimin"))
    } else if (check.num.factors == 0) {flag.keepGoing = FALSE
    } else if (check.num.factors > nQ) {print("Number of factors must be smaller than the number of questions on the assessment.")
    }  else {print("That is not a viable entry.")}
  }
}
