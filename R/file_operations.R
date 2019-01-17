#' @title Open A File With Tasks
#'
#' @author Paul Lemmens (paul.lemmens@gmail.com)
#'
#' @description
#' This function loads tasks from a file provided. This file typically is
#' the `todo.txt` file but can also be a `done.txt` file.
#'
#' @param f An absolute path to a todo.txt file (string).
#'
#' @return A tibble with the tasks loaded from file with variables as
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

  ## Add our package as class to define printing methods.
  class(todo) <- append('todoRtxt', class(todo))

  return(todo)
}


#' @title Write A File With Tasks
#'
#' @author Paul Lemmens (paul.lemmens@gmail.com)
#'
#' @description
#' Writing a set of tasks to file comprises writing all entries in the `task`
#' variable in a tibble produced by [`parse_tasks()`].
#'
#' @param tasks A vector of strings representing tasks following the
#' todo.txt convention by Gina Trapani.
#'
#' @export
write_tasks <- function(tasks, todo_file) {
  stopifnot(length(tasks) > 0)

  ## FIXME: expand to use tryCatch()es and a backup of the original file.
  ## for that use the fs package??

  ## 1. Write new file to temp local location
  ## 2. if successful temp file, then create backup copy of original
  ## 3. overwrite original with new file from temp locatio
  ## 4. remove backup of original

  ## FIXME: check if it is feasible to (first) parse temporary (new) file to
  ## validate new content against old changed content.

  writeLines(tasks, con = todo_file)
}
