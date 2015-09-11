#' List Todo items
#'
#' Lists all to do's or only those matching a regex \code{pattern}. \code{tls()} is an abbreviated form.
#'
#' @param pattern is a regex pattern and all matching to do's are listed. 
#' 
#' @return nothing
#'
#' @export
t.list <- function(pattern = NULL) {
  if (!is.null(pattern)) {
# FIXME: presumes tasks is always the appropriate thing to look for in global env.
    idx <- grepl(pattern, tasks$clean.task, ignore.case = TRUE)
    tasks[idx, ]
  } else {
    tasks
  }
}
#' @export
tls <- function(...) {
  t.list(...)
}

