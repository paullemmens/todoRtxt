#' @title Open A File With To Dos
#'
#' @author Paul Lemmens (paul.lemmens@gmail.com)
#'
#' @description
#' This function loads tasks from a file provided. This file typically is
#' the `todo.txt` file but can also be a `done.txt` file.
#'
#' @param f An absolute path to a todo.txt file (string).
#'
#' @return A tibble with the to dos loaded from file with variables as
#' specified in [parse_prefixes()].
#'
#' @importFrom dplyr "%>%"
#'
#' @export
load_tasks <- function(f) {
  stopifnot(file.exists(f))
  raw <- readLines(con = f, encoding = 'UTF-8')

  ## Parse the various components. For now do use (deprecated) rowwise()
  ## for parsing multi-item returning functions.
  todo <- parse_prefixes(raw) %>%
    dplyr::mutate(date_due       = parse_date(task, prefix = 'due'),
                  date_threshold = parse_date(task, prefix = 't'),
                  recurrence     = parse_recurrence(task))
  todo <- todo %>%
    dplyr::mutate(context = purrr::map(task, ~ parse_tags(., tag = '@')),
                  project = purrr::map(task, ~ parse_tags(., tag = '+')))

  ## FIXME: validate parsed to dos?

  return(todo)
}


