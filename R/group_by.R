#' @title Function Operator To Unnest Context and List Tags
#'
#' @description One task may have multiple lists (`+` prefix) or
#'    context (`@`). These are stored as vectors in a list column
#'    in the data frame with all tasks. This function operator
#'    can be used to modify existing function to ensure proper
#'    unnesting before applying other steps.
#'
#' @param f A function that needs to work on (nested) tags.
#' @param tag A (bare) tibble variable for the column to unnest.
#'
#' @return `f` that first unnests one or more tag columns/variables
#'    before doing its regular work.
#'
#' @export
fo_unnest_tag <- function(f, tag, ...) {
  force(f)

  fo_f <- function(dfr, ...) {
     d <- tidyr::unnest(dfr, cols = {{ tag }} )
     res <- f(d, {{ tag }}, ...)
     return(res)
  }

  return(fo_f)
}


#' @title Group Tasks by Their Context
#'
#' @description Note that grouping by multiple (types of) tags is
#'    accomplished by chaining [`group_by_context`] and
#'    [`group_by_project`].
#'
#' @export
group_by_context <- fo_unnest_tag(f = dplyr::group_by, tag = context)


#' @title Group Tasks by Their List / Project
#'
#' @description Note that grouping by multiple (types of) tags is
#'    accomplished by chaining [`group_by_context`] and
#'    [`group_by_project`].
#'
#' @export
group_by_project <- fo_unnest_tag(f = dplyr::group_by, tag = project)
