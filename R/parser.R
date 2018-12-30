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

  contexts <- dplyr::bind_cols(
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


#' @title Parse Recurrence In A To Do
#'
#' @author Paul Lemmens (paul.lemmens@gmail.com)
#'
#' @description
#' A further extension of the original specification is setting a
#' recurrence of a to do using the prefix `rec:`. A recurrence is set in
#' N days, weeks, months, or years. When N is preceded by a plus, it says to
#' start the recurrence calculation by the original due date or by today's
#' date.
#'
#' @param todo A single to do (string).
#'
#' @return The recurrence setting for a to do as string.
#'
#' @importFrom dplyr "%>%"
parse_recurrence <- function(todo) {
  recurrence_pattern <- ' [Rr][Ee][Cc]:((\\+?)\\d+[dDwWmMyY])'

  rec <- stringr::str_extract(todo, pattern = recurrence_pattern) %>%
    stringr::str_trim() %>%
    stringr::str_replace('[Rr][Ee][Cc]:', '')

  return(rec)
}


#' @title Parse Priority, Creation And Completed Dates
#'
#' @author Paul Lemmens (paul.lemmens@gmail.com)
#'
#' @description
#' Parsing a to do's priority, creation and due date actually combines
#' elements that should be handled separately. However, the grep pattern
#' that is used is quite useful and more difficult to strip into components.
#' Additionally, these components are all prefixed before the actual task
#' so it is sensible to combine parsing in one go.
#'
#' @param todo A to do (string), or a vector of to dos.
#'
#' @return A tibble with variables for creation date, completion date, and
#' priority of a to do
#'
#' @references
#' https://github.com/mpcjanssen/simpletask-android/blob/197bd51f496bd6066df902445acc28df51910d60/src/main/java/nl/mpcjanssen/simpletask/task/Task.java
#'
parse_prefixes <- function(todo) {
  prefix_pattern <- '(^x )?(\\([A-Z]\\) )?(\\d{4}-\\d{2}-\\d{2} +)?(\\d{4}-\\d{2}-\\d{2} +)?(\\d{4}-\\d{2}-\\d{2} +)*(.*)'

  prefixes <- stringr::str_match(todo, pattern = prefix_pattern)

  return(prefixes)
}
