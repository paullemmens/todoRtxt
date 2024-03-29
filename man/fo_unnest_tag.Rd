% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group_by.R
\name{fo_unnest_tag}
\alias{fo_unnest_tag}
\title{Function Operator To Unnest Context and List Tags}
\usage{
fo_unnest_tag(f, tag, ...)
}
\arguments{
\item{f}{A function that needs to work on (nested) tags.}

\item{tag}{A (bare) tibble variable for the column to unnest.}
}
\value{
\code{f} that first unnests one or more tag columns/variables
before doing its regular work.
}
\description{
One task may have multiple lists (\code{+} prefix) or
context (\code{@}). These are stored as vectors in a list column
in the data frame with all tasks. This function operator
can be used to modify existing function to ensure proper
unnesting before applying other steps.
}
