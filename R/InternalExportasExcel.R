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
#' # Pull in PIQL data from AWS and get some course data.
#' PIQLdata <- pullPIQLdata()
#' temp.data <- piql.data.select(PIQLdata, course = 2, numBlanks.allowed = 0)
#' results <- cttDifficulty(temp.data)
#'
#' export.as.excel(results,"filename.xlsx")

export.as.excel <- function(to.be.exported) {
  # Check that file name ends with .xlsx. If not, then add it on.
  filename <- readline(prompt = cat("What would you like to name this save file as (.xlsx will be added automatically)?"))
  if (substr(filename,nchar(filename)-4,nchar(filename)) != ".xlsx") {
    filename <- paste0(filename,".xlsx")
  }
  # Open workbook
  wb <- xlsx::createWorkbook()
  # Sheet names are the names of the given list
  sheets <- names(to.be.exported)

  for(i in 1:length(to.be.exported)){
    # Begin each sheet at row 1
    currRow <- 1
    # Create sheet using the current name in the list.
    sheet <- xlsx::createSheet(wb,sheets[i])
    # Set formatting for row and column names
    cs <- xlsx::CellStyle(wb) + xlsx::Font(wb, isBold=TRUE) + xlsx::Border(position=c("BOTTOM"))
    # Build sheet
    xlsx::addDataFrame(to.be.exported[[i]],
                 sheet=sheet,
                 startRow=currRow,
                 row.names=TRUE,
                 colnamesStyle=cs)

    currRow <- currRow + nrow(to.be.exported[[i]]) + 2
  }
  # Export workbook
  cur.dir <- getwd()
  new.dir <- rstudioapi::selectDirectory()
  setwd(new.dir)
  xlsx::saveWorkbook(wb,file = filename)
  setwd(cur.dir)
}

