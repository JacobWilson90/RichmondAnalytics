#' Load all Champion Data
#'
#' @return A data frame: SSMET
#' @export
load_SeasonStats <- function(
    path = system.file("extdata", "SEASON_SUM_STATS.xlsx", package = "RichmondAnalytics")
) {
  # Read the excel file
  SEASON_SUM_STATS <- readxl::read_excel(path)

  # Export to global environment
  list2env(list(SEASON_SUM_STATS = SEASON_SUM_STATS), envir = .GlobalEnv)

  # Also return it
  return(SEASON_SUM_STATS)
}

