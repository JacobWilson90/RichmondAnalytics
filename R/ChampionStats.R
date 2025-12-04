#' Load all Champion Data
#'
#' @return A list of data frames: CS, CS1, CS2, CS3
#' @export
load_ChampionStats <- function(
    pathChamp = system.file("extdata", "Champion_Stats_V.xlsx", package = "RichmondAnalytics")
) {
  CS  <- readxl::read_excel(pathChamp)

  list2env(list(SSMET = SSMET), envir = .GlobalEnv)
}
