% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filtering.R
\name{extract_tags}
\alias{extract_tags}
\title{Extract Unique Tags}
\usage{
extract_tags(tag_list, type)
}
\arguments{
\item{tag_list}{A list of strings or vectors of strings representing tasks'
tags (with vectors for tasks with multiple tags).}

\item{type}{A string to indicate whether to list contexts or lists.
Should be one of \code{c('context', '@', 'list', 'project', '+')}.}
}
\value{
A vector of (cleaned) tags.
}
\description{
The todo.txt specification uses \verb{@...} to refer to contexts
and \code{+...} for lists. This function extracts all (unique) contexts
or lists from the todo.txt file.
}
