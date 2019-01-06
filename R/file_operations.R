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

  todo <- parse_tasks(raw)
  todo <- todo %>%
    dplyr::mutate(tid = 1:n())


  return(todo)
}


