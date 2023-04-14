#' @title List All Tags
#'
#' @description The todo.txt specification uses `@...` to refer to contexts
#'    and `+...` for lists. This function extracts all (unique) contexts
#'    or lists from the todo.txt file.
#'
#' @param tasks A tibble with tasks as produced by [`parse_tasks`]. **FIXME**
#' @param type A string to indicate whether to list contexts or lists.
#'    Should be one of `c('context', '@', 'list', 'project', '+')`.
#'
#' @return A vector of (cleaned) tags.
#'
#' @importFrom dplyr "%>%"
#'
#' @export
list_tags <- function(tasks, type) {
  res <- dplyr::case_when(
    type == '@'       ~ purrr::list_flatten(tasks$context),
    type == 'context' ~ purrr::list_flatten(tasks$context),
    type == '+'       ~ purrr::list_flatten(tasks$project),
    type == 'list'    ~ purrr::list_flatten(tasks$project),
    type == 'project' ~ purrr::list_flatten(tasks$project),
  )
  res <- purrr::list_c(res)

  return(unique(res))
}
