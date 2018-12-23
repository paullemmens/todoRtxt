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
#' @export
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

