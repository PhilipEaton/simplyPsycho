#' Choose Save Location
#'
#' @description Opens a directory GUI to help the user select a save location.
#'
#' @return Enables users to select directories for saving analysis results.
#'
#' @export
#'
ensure_library = function (lib.name){
  x = require(lib.name, quietly = TRUE, character.only = TRUE)
  if (!x) {
    install.packages(lib.name, dependencies = TRUE, quiet = TRUE)
    x = require(lib.name, quietly = TRUE, character.only = TRUE)
  }
  x
}

select_directory_method = function() {
  # Tries out a sequence of potential methods for selecting a directory to find one that works
  # The fallback default method if nothing else works is to get user input from the console
  if (!exists('.dir.method')){  # if we already established the best method, just use that
    # otherwise lets try out some options to find the best one that works here
    if (exists('utils::choose.dir')) {
      .dir.method = 'choose.dir'
    } else if (rstudioapi::isAvailable() & rstudioapi::getVersion() > '1.1.287') {
      .dir.method = 'RStudioAPI'
      ensure_library('rstudioapi')
    } else if(ensure_library('tcltk') &
              class(try({tt  <- tktoplevel(); tkdestroy(tt)}, silent = TRUE)) != "try-error") {
      .dir.method = 'tcltk'
    } else if (ensure_library('gWidgets2') & ensure_library('RGtk2')) {
      .dir.method = 'gWidgets2RGtk2'
    } else if (ensure_library('rJava') & ensure_library('rChoiceDialogs')) {
      .dir.method = 'rChoiceDialogs'
    } else {
      .dir.method = 'console'
    }
    assign('.dir.method', .dir.method, envir = .GlobalEnv) # remember the chosen method for later
  }
  return(.dir.method)
}

choose_directory <- function(ini_dir = getwd(),
                             method = select_directory_method(),
                             title = 'Select data directory') {
  switch(method,
         'choose.dir' = choose.dir(default = ini_dir, caption = title),
         'RStudioAPI' = selectDirectory(path = ini_dir, caption = title),
         'tcltk' = tk_choose.dir(default = ini_dir, caption = title),
         'rChoiceDialogs' = rchoose.dir(default = ini_dir, caption = title),
         'gWidgets2RGtk2' = gfile(type = 'selectdir', text = title, initial.dir = ini_dir),
         readline('Please enter directory path: ')
  )
}
