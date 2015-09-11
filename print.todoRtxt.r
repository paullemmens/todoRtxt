#' Printing a task list
#'
#' To make sure that the todoRtxt class, inheriting from data.frame, is properly printed on screen,
#' a new method is defined that sorts the tasks on their index/line number in the source file. It then
#' prints them to the screen with each line prefixed with that index number.
#'
#' @param completed A boolean to indicated whether completed tasks should be printed or not (default is yes, print them).
#'
#' @return Returns invisibly.
#'
#' @export
print.todoRtxt <- function(x, completed = TRUE, ...) {
# FIXME: probably want to make this (also) into an (global) option.
  
# Figure out how to pad shorter task IDs.
  padding = nchar(max(x$idx))

# Complicated chain of %>% that I got from my question on SO:
# http://stackoverflow.com/questions/32416948/reverse-ordering-dplyrs-group-by
  d <- x %>% arrange(idx) 

# When completed flag is false, remove completed tasks from the list.
  if (!completed) {
    d <- filter(d, is.na(done))
  }

# Create a line and cat() to the terminal.
  d %>% summarize(cat.task = paste0(paste0(stringr::str_pad(idx, width = padding, side = 'left'), 
                                    ':  ', 
                                    orig.task) %>% unique %>% paste(collapse = '\n'), 
                                    '\n')) %>%
    use_series(cat.task) %>% 
    cat(fill = TRUE, sep = '')

# Wrap up
  return(invisible())
}

#FIXME: If you’re implementing more complicated print() methods, it’s a better idea to
#implement format() methods that return a string, and then implement print.class
#<- function(x, ...) cat(format(x, ...), "\n". This makes for methods that are
#                        much easier to compose, because the side-effects are
#                        isolated to a single place
# from hadley's S3 guide

print <- function(x, ...) UseMethod('print', x)

print.default <- function(x) return(x)

