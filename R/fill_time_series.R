#' @title Complete Years Time Series
#'
#' @description Fills gaps in time series data of years that might occur due to missing
#' data or dates in task lists.
#'
#' @param dfr A data frame with a column for year numbers.
#' @param year_col A bare column name (defaults to year) to expand to a full
#'    sequence of year numbers.
#'
#' @return The expanded `dfr` with a column variable `year_col` that contains
#'    a full sequence of year numbers.
#'
#' @importFrom dplyr %>%
#
#' @export
complete_years <- function(dfr, year_col = year) {

  years_observed <- dplyr::distinct(dfr, {{ year_col}} ) %>% pull( {{ year_col}} )
  all_years <- tidyr::full_seq(years_observed, period = 1)

  full_dfr <- dplyr::full_join(x = dfr,
                               y = tibble( {{ year_col }} := all_years),
                               ## Use non-idiomatic way because idiomatic rlang won't fly
                               by = deparse(substitute(year_col)))

  return(full_dfr)
}
