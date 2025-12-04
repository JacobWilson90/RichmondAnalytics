#' Load all Champion Data
#'
#' @return A data frame: SSMET
#' @export
load_Metrics <- function(
    pathMetrics = system.file("extdata", "Metrics26.xlsx", package = "RichmondAnalytics")
) {
  # Read the excel file
  SSMET <- readxl::read_excel(pathMetrics)

  # Export to global environment
  list2env(list(SSMET = SSMET), envir = .GlobalEnv)

  # Also return it
  return(SSMET)
}
