#' Export to Spreadhseet
#'
#' @description Export the outputs of simplyPsycho function as an Excel Spreadsheet
#'
#' @param to.be.exported Output from one of simplyPsycho's analysis functions.
#'
#' @param filename Name of file when saved to the working directory.
#' (The .xlsx is added automatically.)
#'
#' @return Saves an .xlsx to the current working directory.
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

export.as.excel <- function(to.be.exported, filename) {
  # Check that file name ends with .xlsx. If not, then add it on.
  if (substr(filename,nchar(filename)-4,nchar(filename)) != ".xlsx") {
    filename <- paste0(filename,".xlsx")
  }
  # Open workbook
  wb <- xlsx::createWorkbook()
  # Sheet names are the names of the given list
  sheets <- names(theList)

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
  xlsx::saveWorkbook(wb,file = filename)
}

