#' Load all Champion Data
#'
#' @return A list of data frames: CS, CS1, CS2, CS3
#' @export
load_MetricCats <- function() {
  CS  <- readxl::read_excel(pathChamp)
  SSMET <- readxl::read_excel(pathMetrics)

  CS1 <- CS %>%
    dplyr::filter(
      champion.stat.code %in% SSMET$Metric_Name |
        champion.stat.code %in% c(
          'KICK','EFFECTIVE KICK','SHOT AT GOAL','GOAL','BEHIND','RUSHED BEHIND',
          'THROW IN HITOUT','BALL UP HITOUT','BU HITOUT TO ADVANTAGE',
          'TI HITOUT TO ADVANTAGE','BALL UP HITOUT SHARKED',
          'THROW IN HITOUT SHARKED','BALL UP FIRST POSSESSION',
          'THROW IN FIRST POSSESSION','THROW IN CLEARANCE',
          'BALL UP CLEARANCE','TACKLE','MISSED TACKLES'
        )
    )

  CS2 <- CS %>%
    dplyr::filter(
      champion.stat.code %in% c(
        "DISPOSAL","CENTRE BOUNCE","THROW IN","BALL UP","UNCONTESTED POSSESSION",
        "CONTESTED POSSESSION","DISPOSAL","KICK IN","OUT ON FULL","FREE FOR",
        "BEHIND","RUSHED BEHIND","SPOIL","INSIDE 50 RESULT","ONE","CONTEST TARGET",
        "OUT OF BOUNDS","BALL UP CALL","GOAL","DROPPED MARK","NO PRESSURE ERROR",
        "KNOCK ON","CENTRE BOUNCE","BALL UP","THROW IN"
      )
    )

  CS3 <- CS %>%
    dplyr::filter(
      champion.stat.code %in% c(
        'HITOUT','HIT OUT TO ADVANTAGE','HIT OUT SHARKED','FIRST POSSESSION',
        'CLEARANCE','MARK','TACKLE','CONTESTED POSSESSION','GROUND BALL GET',
        'MISSED TACKLES','FREE AGAINST'
      )
    )

  list2env(list(CS = CS, CS1 = CS1, CS2 = CS2, CS3 = CS3), envir = .GlobalEnv)
}
