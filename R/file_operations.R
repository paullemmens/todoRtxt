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
    dplyr::mutate(tid = 1:dplyr::n())

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

  ## 1. Write new file to temp local location
  tempfile <- fs::file_create(fs::file_temp())
  if (fs::file_exists(tempfile)) {
    writeLines(tasks, con = tempfile)
  }

  ## 2. if successful temp file, then create backup copy of original
  ## FIXME: check if it is feasible to (first) parse temporary (new) file to
  ## validate new content against old changed content.
  bakfile <- fs::file_temp()
  tryCatch({
    fs::file_copy(todo_file, bakfile)
  }, warning = function(warn) {
      message('Something did not work out making a backup of the original file.')
      message(warn)
      return(NULL)
  }, error = function(err) {
      message('Something did not work out making a backup of the original file.')
      message(err)
      return(NA)
  })

  ## 3. overwrite original with new file from temp location
  ## catching errors to prevent losing info.
  tryCatch({
    fs::file_copy(tempfile, todo_file, overwrite = TRUE)
  },
  warning = function(war) {
    message('Warning during writing todo file. Restoring backup.')
    message(warn)
    fs::file_copy(bakfile, todo_file, overwrite = TRUE)
    return(NULL)
  },
  error = function(err) {
    message('Error during writing todo file. Restoring backup.')
    message(err)
    fs::file_copy(bakfile, todo_file, overwrite = TRUE)
    return(NA)
  })

  ## 4. remove backup of original
  fs::file_delete(c(tempfile, bakfile))

  return(TRUE)
}


