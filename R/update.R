#' @title Modify Task Property
#'
#' @author Paul Lemmens (paul.lemmens@gmail.com)
#'
#' @description
#' This is a package internal function used to modify a task given an
#' input and a task to be modified. Because the tasks all are simple
#' textual strings, modification is implemented by text substitution and
#' then re-parsing of the task.
#'
#' The function presumes that all modifications of properties can be based
#' on strings like "prop:value" (wherein underscores represent a space).
#' Thus, strictly speaking, tags are not properties of a task.
#'
#' To implement a degree of flexibility in how to specify properties and
#' values, `prop` defaults to NULL so that `value` can (also) be used to
#' specify new values as `prop:value`.
#'
#' Also in view of flexibility, the function presumes that the user ensures
#' to maintain the todo.txt specification. So no checking is done to
#' ascertain whether a property is a valid property of the (original)
#' specification.
#'
#' @param task A string with one (or, as vector, one or more) task(s) that
#'     follow(s) the todo.txt specification by Gina Trapani. When multiple 
#'     tasks are provided, *all* tasks are modified in the same way.
#' @param prop Identifier of the property of the task (a string). Can be
#'     postfixed with a colon (e.g. "due:" instead of "due"). Defaults to
#'     NULL implying that then `value` needs to be specified as
#'     `property:value` instead of only `value`.
#' @param value New value for the property (a string).
#'
#' @return The updated task(s)
#'
#' @name modify_property
modify_property <- function(task, prop = NULL, value) {

  if (is.null(prop)) {
    prop <- stringr::str_split(value, ':', simplify = TRUE)[1, 1]
  } else {
    if (stringr::str_sub(prop, start = -1, end = -1) == ':') {
      value <- paste0(prop, value)
    } else {
      value <- paste0(prop, ':', value)
    }
  }

  ## TODO: ugly, redoes stringr::str_sub() from above which makes errors likely
  prop <- dplyr::if_else(stringr::str_sub(prop, -1, -1) == ':', prop, paste0(prop, ':'))

  task <- stringr::str_replace(task,
                               pattern     = paste0(prop, '[:graph:]+'),
                               replacement = value)

  return(task)
}


