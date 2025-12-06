#' Formatting the values.
#'
#' Creates a display value and the output value.
#'
#' @param df A data frame produced by `SEASON_SUM_STATS3()` in stat ladders.
#'
#' @return The same data frame with diplay and output values.
#' @export
#'
add_Formatting <- function(df) {

tempdf <- df

FT3 <- tempdf %>%
  mutate(Data_Type = as.numeric(Data_Type)) %>%
  mutate(
    A = Data_Type %/% 10000, B = (Data_Type %/% 1000) %% 10, C = (Data_Type %/% 100) %% 10, D = (Data_Type %/% 10) %% 10, E = Data_Type %% 10) %>%
  mutate(
    val = case_when(
      A == 1 ~ as.numeric(.[[2]]),                                    # For
      A == 2 ~ as.numeric(.[[3]]),                                    # Against
      A == 3 ~ as.numeric(.[[2]]) - as.numeric(.[[3]]),               # Difference
      A == 4 ~ as.numeric(.[[2]]) - as.numeric(.[[12]]),              # Zone diff (your old special case)
      A == 5 ~ as.numeric(.[[2]]),                                   # Special
      A == 6 ~ as.numeric(.[[3]]),                                   # Special
      TRUE ~ NA_real_)) %>%
  mutate(
    val = case_when(
      B == 1 ~ val,                                                   # Raw
      B == 2 ~ val * 100,
      B == 3 ~ val * 100,   # Percent
      TRUE ~ val)) %>%
  mutate(
    val = case_when(
      E == 1 ~ val,
      E == 2 ~ val / as.numeric(.[[4]]),
      TRUE  ~ val)) %>%
  mutate(
    Rank1 = case_when(
      D == 1 ~ rank(val, ties.method = "random"),       # Smallest = best
      D == 2 ~ rank(-val, ties.method = "random"),      # Largest = best
      TRUE   ~ rank(val, ties.method = "random"))) %>%
  mutate(
    val = case_when(
      C == 0 ~ round(val, 0),
      C == 1 ~ round(val, 1),
      C == 2 ~ round(val, 2),
      TRUE ~ val)) %>%
  mutate(
    Display1 = case_when(
      A %in% c(3,4) ~ case_when(val >= 0.001 ~ paste0('+',val), TRUE ~ as.character (val)),     # Smallest = best
      A %in% c(5) ~ paste0(val," - ",as.numeric(.[[11]])),      # Largest = best
      A %in% c(6) ~ paste0(val," - ",as.numeric(.[[12]])),      # Largest = best
      TRUE ~ as.character(val))) %>%
  mutate(Display1 = case_when(
    B == 2 ~ paste0 (Display1,'%'), TRUE ~ Display1)) %>%
  mutate(Output1 = val) %>%

  mutate(
    val = case_when(
      A == 1 ~ as.numeric(.[[5]]),                                    # For
      A == 2 ~ as.numeric(.[[6]]),                                    # Against
      A == 3 ~ as.numeric(.[[5]]) - as.numeric(.[[6]]),               # Difference
      A == 4 ~ as.numeric(.[[5]]) - as.numeric(.[[14]]),              # Zone diff (your old special case)
      A == 5 ~ as.numeric(.[[5]]),                                   # Special
      A == 6 ~ as.numeric(.[[6]]),
      TRUE ~ NA_real_)) %>%
  mutate(
    val = case_when(
      B == 1 ~ val,                                                   # Raw
      B == 2 ~ val * 100,                                             # Percent
      B == 3 ~ val * 100,
      TRUE ~ val)) %>%
  mutate(
    val = case_when(
      E == 1 ~ val,
      E == 2 ~ val / as.numeric(.[[7]]),
      TRUE  ~ val)) %>%
  mutate(
    Rank2 = case_when(
      D == 1 ~ rank(val, ties.method = "random"),       # Smallest = best
      D == 2 ~ rank(-val, ties.method = "random"),      # Largest = best
      TRUE   ~ rank(val, ties.method = "random"))) %>%
  mutate(
    val = case_when(
      C == 0 ~ round(val, 0),
      C == 1 ~ round(val, 1),
      C == 2 ~ round(val, 2),
      TRUE ~ val)) %>%
  mutate(
    Display2 = case_when(
      A %in% c(3,4) ~ case_when (val >= 0.001  ~ paste0('+',val), TRUE ~ as.character (val)),     # Smallest = best
      A %in% c(5) ~ paste0(val," - ",as.numeric(.[[13]])),      # Largest = best
      A %in% c(6) ~ paste0(val," - ",as.numeric(.[[14]])),      # Largest = best
      TRUE ~ as.character(val))) %>%
  mutate(Display2 = case_when(
    B == 2 ~ paste0 (Display2,'%'), TRUE ~ Display2)) %>%
  mutate(Output2 = val) %>%

  mutate(
    val = case_when(
      A == 1 ~ as.numeric(.[[8]]),                                    # For
      A == 2 ~ as.numeric(.[[9]]),                                    # Against
      A == 3 ~ as.numeric(.[[8]]) - as.numeric(.[[9]]),               # Difference
      A == 4 ~ as.numeric(.[[8]]) - as.numeric(.[[16]]),              # Zone diff (your old special case)
      A == 5 ~ as.numeric(.[[8]]),                                   # Special
      A == 6 ~ as.numeric(.[[9]]),
      TRUE ~ NA_real_)) %>%
  mutate(
    val = case_when(
      B == 1 ~ val,                                                   # Raw
      B == 2 ~ val * 100,
      B == 3 ~ val * 100,   # Percent
      TRUE ~ val)) %>%
  mutate(
    val = case_when(
      E == 1 ~ val,
      E == 2 ~ val / as.numeric(.[[10]]),
      TRUE  ~ val)) %>%
  mutate(
    Rank3 = case_when(
      D == 1 ~ rank(val, ties.method = "random"),       # Smallest = best
      D == 2 ~ rank(-val, ties.method = "random"),      # Largest = best
      TRUE   ~ rank(val, ties.method = "random"))) %>%
  mutate(
    val = case_when(
      C == 0 ~ round(val, 0),
      C == 1 ~ round(val, 1),
      C == 2 ~ round(val, 2),
      TRUE ~ val)) %>%
  mutate(
    Display3 = case_when(
      A %in% c(3,4) ~ case_when (val >= 0.001  ~ paste0('+',val), TRUE ~ as.character (val)),     # Smallest = best
      A %in% c(5) ~ paste0(val," - ",as.numeric(.[[15]])),      # Largest = best
      A %in% c(6) ~ paste0(val," - ",as.numeric(.[[16]])),      # Largest = best
      TRUE ~ as.character(val))) %>%
  mutate(Display3 = case_when(
    B == 2 ~ paste0 (Display3,'%'), TRUE ~ Display3)) %>%
  mutate(Output3 = val)


return (FT3)

}
