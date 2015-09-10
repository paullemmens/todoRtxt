#' Parse todo.txt (akin) file(s)
#'
#' Parses the todo txt file to search for due dates, threshold dates, priorities, date entered, completed y/no,
#' and date of completion.
#'
#' @param raw.todo Unparsed contents from a todo.txt file having been read by read.todo().
#'
#' @return A data frame containing a parse todo.txt file of class data.frame and \code{todotxt}. The data frame
#' contains the original (unparsed) task(s), tasks cleaned from parsed items, and the parsed items of completed
#' (\code{done}), date completed (\code{d.compl}), date entered (\code{d.ent}), priority (\code{prio}), a
#' comma-separated list of tags and/or lists, and the recurrence pattern.
#'
#' @export
parse.todotxt <- function(raw.todo) {

# This file in the repo of Simpletask cloudless shows the patterns that author 
# uses to get thing working.
# https://github.com/mpcjanssen/simpletask-android/blob/197bd51f496bd6066df902445acc28df51910d60/src/main/java/nl/mpcjanssen/simpletask/task/Task.java
  prefix.map <- data.frame(str_match(todo.raw, '(^x )?(\\([A-Z]\\) )?(\\d{4}-\\d{2}-\\d{2} +)?(\\d{4}-\\d{2}-\\d{2} +)?(\\d{4}-\\d{2}-\\d{2} +)*(.*)'),
                           stringsAsFactors = FALSE)
  names(prefix.map)[-6] <- c('orig.task', 'done', 'prio', 'd.compl', 'd.ent', 'clean.task')
  prefix.map$line <- 1:nrow(prefix.map)
  prefix.map$idx <- 1:nrow(prefix.map)

# A series of maps to tease out relevant bits from each line.
  due.ptrn <- '( [Dd][Uu][Ee]:\\d{4}-\\d{2}-\\d{2})'
  due.dates <- as.Date(gsub('due:', '', str_extract(todo.raw, due.ptrn)))
  thr.ptrn <- '( [Tt]:\\d{4}-\\d{2}-\\d{2})'
  thr.dates <- as.Date(gsub('t:', '', str_extract(todo.raw, thr.ptrn)))
  rec.ptrn <- '( [Rr][Ee][Cc]:\\d+[dDwWmMyY])'
# FIXME: rec.dates not really correct name because 'rec:1m' is not a date.
  rec.dates <- str_extract(todo.raw, rec.ptrn)

# This gives a list that needs is flattened down.
  tags <- get.strings(todo.raw, '( \\+\\w+)')
  lists <- get.strings(todo.raw, '( \\@\\w+)')

# Combine everything in one big data.frame.
  tasks <- cbind(prefix.map, 
                 data.frame(due = due.dates, threshold = thr.dates, 
                            recurrence = rec.dates, tags = tags, lists = lists,
                            stringsAsFactors = FALSE))

# FIXME: sort file on something?

# Return the tasks with another class identifier added.
  class(tasks) <- append('todoRtxt', class(tasks))
  return(tasks)
}


#' Parse tags/projects and lists/context
#'
#' Helper function to create an array of strings containing the list and tag 
#' identifiers. This array is then later added to the main data frame containing
#' all todos. 
#'
#' The function only works to create a more manageable thing out of a map that we
#' got via a stringr function.
#'
#' @param string String (or vector of strings) to parse for pattern.
#' @param pattern Pattern to search string for.
#'
#' @return An array of comma separated patterns that were extracted from string with 
#' empty rows for non-matching string rows if string is a vector/array.
#'
get.strings <- function(string, pattern) {
  map <- stringr::str_extract_all(string, pattern)
  res <- do.call('c',
                 lapply(map, 
                        function(mm) {
                          ifelse(length(mm) == 0, 
                                 '', 
                                 gsub(' ', '', paste0(mm, collapse = ',')))
                        }))
  return(res)
}
