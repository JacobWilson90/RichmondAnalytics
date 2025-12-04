#' Load all Champion Data
#'
#' @return A list of data frames: CS, CS1, CS2, CS3
#' @export
load_Metrics <- function(
    pathMetrics = system.file("extdata", "Metrics26.xlsx", package = "RichmondAnalytics")
) {
  CS  <- readxl::read_excel(pathMetrics)

  list2env(list(SSMET = SSMET), envir = .GlobalEnv)
}
