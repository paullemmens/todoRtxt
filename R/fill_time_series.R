#' @title Complete Year-Week Time Series
#'
#' Fills gaps in time series data that might occur due to missing data
#' or dates in task lists.
#'
#' @param tasks A list of tasks that has a year and week variable.
#' @param date_col A bare column to do calculations on to create
#'    reference calendar.
#'
#' @return An expanded list of tasks with year and week number variables
#'    that is complete for 52 (or 53) weeks per each of the years.
#'
#' @importFrom dplyr %>%
#
#' @export
complete_year_week <- function(tasks, date_col) {

  min_year <- lubridate::year(min(tasks[ {{ date_col }} ]))
  max_year <- lubridate::year(max(tasks[ {{ date_col }} ]))
  cal <- tasks %>%
    dplyr::select( {{ date_col }} ) %>%
    dplyr::complete(cal = seq.Date(from = lubridate::ymd(paste(c(min_year, 1, 1), collapse = '-')),
                                   to   = lubridate::ymd(paste(c(min_year, 12, 31), collapse = '-')),
                                   by   = 'day'))

  return(full_join(x = tasks, y = cal, by = {{ date_col }} ))
}
