#' @title Modify A Tasks
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
#' @param prop Identifier of the property of the task (a string).
#' @param value New value for the property.
#'
#' @return The updated task(s)
#'
modify_task <- function(task, prop = NULL, value) {

  if (is.null(prop)) {
    prop <- stringr::str_split(value, ':', simplify = TRUE)[1, 1]
  } else {
    value <- paste0(prop, ':', value)
  }

  task <- stringr::str_replace(task,
                               pattern     = paste0(' ', prop, ':\\w+'),
                               replacement = paste0(' ', value, ' '))

  return(task)
}


