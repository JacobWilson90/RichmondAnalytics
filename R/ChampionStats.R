#' Load all Champion Data
#'
#' @return A data frame: SSMET
#' @export
load_Champion <- function(
    pathChamp = system.file("extdata", "Champion_Stats_V.xlsx", package = "RichmondAnalytics")
) {
  # Read the excel file
  CS  <- readxl::read_excel(pathChamp)

  # Export to global environment
  list2env(list(CS = CS), envir = .GlobalEnv)

  # Also return it
  return(CS)
}
