#' @title Parse Tags From A To Do
#'
#' @author Paul Lemmens (paul.lemmens@gmail.com)
#'
#' @description
#' Contexts are tags prefixed with an `@`. Projects are tags prefixed with
#' a `+` (plus). One to do can contain multiple
#' contexts and/or multiple projects that should all be preserved.
#' The regular expressions are taken from the source code of Simpletask by
#' Mark Janssen.
#'
#' @param todo A single todo (string).
#' @param tag A plus `+` or `@` sign (string)
#'
#' @return A (mini) data frame with a row for each context found and in that
#'     row a variable for the context and its start and end position in the
#'     to do.
#'
#' @importFrom dplyr "%>%"
#'
parse_tags <- function(todo, tag) {
  contexts_pattern <- paste0('( \\', tag, '\\w+)')

  contexts <- stringr::str_extract_all(todo, pattern = contexts_pattern)[[1]] %>%
    stringr::str_trim()
  positions <- stringr::str_locate_all(todo, pattern = contexts_pattern)[[1]]

  contexts <- bind_cols(
    tibble::tibble(context = contexts),
    tibble::tibble(start   = positions[, 'start'],
                   end     = positions[, 'end']))

  return(contexts)
}


#' @title Parse Date Components Of A To Do
#'
#' @author Paul Lemmens (paul.lemmens@gmail.com)
#'
#' @description
#' Following the official definition from Gina Trappani, a to do can have a due
#' date only. This due date is indicated with the string `due:` immediately
#' (i.e., no space) followed by a date in ISO format (yyyy-mm-dd).
#'
#' Extensions of the todo.txt format also specify a start date for a task also
#' frequently referred to as threshold date: the date when a task should appear
#' on a list or in a view. Typically `t:` is used for indicating a threshold
#' date.
#'
#' Due dates and threshold dates should appear only once in any to do. So only
#' the first occurrence is considered.
#'
#' @param todo A single todo (string).
#' @param prefix String to indicate which type of date to search: either `t`
#'     or `due`; defaults to 'due'.
#'
#' @return The due or threshold date that was found after checking whether it
#' is a valid date stamp; if it is not, NA is returned.
#'
#' @importFrom dplyr "%>%"
#'
parse_date <- function(todo, prefix = 'due') {
  date_pattern <- '( [Dd][Uu][Ee]:\\d{4}-\\d{2}-\\d{2})'
  if (prefix == 't') {
    date_pattern <- '( [Tt]:\\d{4}-\\d{2}-\\d{2})'
  }

  dt <- stringr::str_extract(todo, pattern = date_pattern) %>%
    stringr::str_trim() %>%
    stringr::str_replace('([Dd][Uu][Ee]|[Tt]):', '')

  return(lubridate::ymd(dt, quiet = TRUE))
}
