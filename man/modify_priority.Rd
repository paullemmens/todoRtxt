% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update.R
\name{modify_priority}
\alias{modify_priority}
\title{Modify Task Priority}
\usage{
modify_priority(task, priority)
}
\arguments{
\item{task}{A string with one (or, as vector, one or more) task(s) that
follow(s) the todo.txt specification by Gina Trapani. When multiple
tasks are provided, \emph{all} tasks are modified in the same way.}

\item{priority}{A string with the new priority to set. If a priority needs
to be removed, use the empty string.}
}
\value{
the updated task(s)
}
\description{
In the todo.txt format, tasks can be prioritized using a capital in round
braces at the start of the task.
}
\author{
Paul Lemmens (paul.lemmens@gmail.com)
}
