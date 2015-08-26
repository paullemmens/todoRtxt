#' Load todo.txt file
#'
#' Loads the todo.txt file from a specified location. Principally, this function is not exported, because I want to find a way to automagically 
#' do this when the package is being loaded using \code{library()} or \code{require()}.
#'
#' @param path Path where the file is located. Should be a local path; that is, (windows) mounted paths can be used too.
#' @param todo.file Filename of the todo.txt file. Defaults to 'todo.txt'.
#'
#' @return The raw output from a call to \code{readlines()}.
#'
#' @export
load.todo <- function(path, todo.file = 'todo.txt') {
  todo.raw <- readLines(con = file.path(todo.dir, 'todo.txt'), encoding = 'UTF-8') 
}


#' Load done.txt file
#'
#' Loads the done.txt file from a specified location. This function is not exported because it is internally used when performing analyses on the data in todo.txt
#' and done.txt.
#'
#' @param path Path where the file is located. Should be a local path; that is, (windows) mounted paths can be used too.
#' @param todo.file Filename of the todo.txt file. Defaults to 'todo.txt'.
#'
#' @return The raw output from a call to \code{readlines()}.
#'
load.done <- function(path, todo.file = 'todo.txt') {
  done.raw <- readLines(con = file.path(todo.dir, 'done.txt'), encoding = 'UTF-8') 
}
