#' @title Parse Contexts In A Todo
#'
#' @description
#' Contexts are tags prefixed with an `@`. One todo can contain multiple
#' contexts that should all be preserved. The regular expression is taken
#' from the source code of Simpletask by Mark Janssen.
#'
#' @param todo A single todo (string).
#'
#' @return A (mini) data frame with a row for each context found and in that
#'     row a variable for the context and its start and end position in the
#'     todo.
#'
#' @importFrom dplyr "%>%"
#'
#' @export
parse_contexts <- function(todo) {
  contexts_pattern <- '( \\@\\w+)'

  contexts <- stringr::str_extract_all(todo, pattern = contexts_pattern)[[1]] %>%
    stringr::str_trim()
  positions <- stringr::str_locate_all(todo, pattern = contexts_pattern)[[1]]

  contexts <- bind_cols(
    tibble::tibble(context = contexts),
    tibble::tibble(start   = positions[, 'start'],
                   end     = positions[, 'end']))

  return(contexts)
}
