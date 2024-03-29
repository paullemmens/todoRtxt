#' @title Extract Unique Tags
#'
#' @description The todo.txt specification uses `@...` to refer to contexts
#'    and `+...` for lists. This function extracts all (unique) contexts
#'    or lists from the todo.txt file.
#'
#' @param tag_list A list of strings or vectors of strings representing tasks'
#'    tags (with vectors for tasks with multiple tags).
#' @param type A string to indicate whether to list contexts or lists.
#'    Should be one of `c('context', '@', 'list', 'project', '+')`.
#'
#' @return A vector of (cleaned) tags.
#'
#' @export
extract_tags <- function(tag_list, type) {

  stopifnot(type %in% c('@', 'context', '+', 'list', 'project'))

  res <- purrr::list_flatten(tag_list)
  res <- purrr::list_c(res)

  if (type %in% c('@', 'context')) {
    res <- res[which(grepl('^@', res))]
  } else if (type %in% c('+', 'list', 'project')) {
    res <- res[which(grepl('^\\+', res))]
  }

  ## Explicitly/deliberately set NULL results to NA.
  if (length(res) == 0)
    res <- NA_character_

  return(unique(res))
}


#' @title Find Index of Tag in List of Tags
#'
#' @description
#'    Finds the index of a particular tag in a list of tags wherein the
#'    list might also contain vectors. This function helps to find row
#'    indices for later filtering functions.
#'
#' @inheritParams extract_tags
#' @param tag Simple string of one tag to find in the `tag_list`.
#'
#' @return A boolean vector for the list indices that contain `tag`.
#'
#' @examples
#' todo <- c('x 2018-12-12 2018-12-10 x y z @home +list',
#'           '(A) 2018-12-10 a b c @home @work',
#'           '2018-12-12 def +list',
#'           'a b c @context +list +project')
#' parse_tasks(todo) %>% filter(find_tag(context, '@home'))
#'
#' @export
find_tag <- function(tag_list, tag) {

  purrr::map_lgl(tag_list, ~ purrr::has_element(.x = .x, .y = tag))
}
