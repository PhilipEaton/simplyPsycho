#' Export to Spreadsheet
#'
#' @description Export the outputs of simplyPsycho function as an Excel Spreadsheet
#'
#' @param to.be.exported Output from one of simplyPsycho's analysis functions.
#'
#' @return Saves an .xlsx to the current working directory.
#'
#' @import rstudioapi
#'
#' @export
#'
#' @examples
#' # Pull sample data
#' temp.data <- piql.data.select(simplySampleData, courses = 1, numBlanks.allowed = 0)
#' results <- cttDifficulty(temp.data)
#'
#' export.as.excel(results,"filename.xlsx")

export.as.excel <- function(to.be.exported) {
  # -------------------------------------------------------------------------- #
  # Check that file name ends with .xlsx. If not, then add it on.
  filename <- readline(prompt = cat("What would you like to name this save file as (.xlsx will be added automatically)?"))
  if (substr(filename,nchar(filename)-4,nchar(filename)) != ".xlsx") {
    filename <- paste0(filename,".xlsx")
  }
  # -------------------------------------------------------------------------- #
  # Get sheet names from names of the given list
  sheets <- names(to.be.exported)
  workBook <- list()

  for(i in 1:length(to.be.exported)){
    ## Create Workbook.
    workBook[[i]] <- as.data.frame(cbind(as.data.frame(rownames(to.be.exported[[i]])),to.be.exported[[i]]))
    names(workBook[[i]]) <- c("Name", colnames(to.be.exported[[i]]))
    names(workBook)[i] <- c(sheets[i])
    }
  # Export workbook
  cur.dir <- getwd()
  new.dir <- choose_directory()
  setwd(new.dir)
  writexl::write_xlsx( workBook, path = paste0(new.dir,"/",filename) )
  setwd(cur.dir)
}

