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

#' @title Filter Tasks On Contexts or Lists/Projects
#'
#' @description Tasks contain (multiple) tags as a list. This function
#'    enables filtering these tasks on particular tags. Tags here are
#'    either contexts (prefixed with an `@`) or lists (also referred to
#'    as lists with `+`).
#'
#'    Proposed usage (to explain the naming) is
#'    `dplyr::filter(on_context('@home'))` or similarly
#'    `on_tag('+list')`.
#'
#' @param pattern Pattern to filter on. Should use the common prefix of
#'    each tag (i.e. for contexts the `@` and lists/projects a `+`).
#' @param type An indicator for the type of tag.
#'
#' @return A boolean vector of length of the number of rows in tasks.
#'
#' @importFrom dplyr "%>%"
#'
on_tag <- function(pattern) {
}

#' @export
on_context <- function(pattern) {
  on_tag(pattern = pattern, type = 'context')
}

#' @export
on_list <- function(pattern) {
  on_tag(pattern = pattern, type = 'list')
}
on_project <- on_list
