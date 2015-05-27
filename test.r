setwd('c:/nlv19419/R/todoRtxt')
todo.dir <- 'C:/Users/NLV19419/Google Drive/todo'
library(stringr)

todo.raw <- readLines(con = file.path(todo.dir, 'todo.txt'), encoding = 'UTF-8') 
done.raw <- readLines(con = file.path(todo.dir, 'done.txt'), encoding = 'UTF-8') 

# This file in the repo of Simpletask cloudless shows the patterns that author 
# uses to get thing working.
# FIXME: retrieve link.
prefix.map <- data.frame(str_match(todo.raw, '(^x )?(\\([A-Z]\\) )?(\\d{4}-\\d{2}-\\d{2} +)?(\\d{4}-\\d{2}-\\d{2} +)?(\\d{4}-\\d{2}-\\d{2} +)*(.*)'),
                         stringsAsFactors = FALSE)
names(prefix.map)[-6] <- c('orig.task', 'done', 'prio', 'd.compl', 'd.ent', 'clean.task')

# A series of maps to tease out relevant bits from each line.
due.ptrn <- '( [Dd][Uu][Ee]:\\d{4}-\\d{2}-\\d{2})'
due.dates <- str_extract(todo.raw, due.ptrn)
thr.ptrn <- '( [Tt]:\\d{4}-\\d{2}-\\d{2})?'
thr.dates <- str_extract(todo.raw, thr.ptrn)
rec.ptrn <- '( [Rr][Ee][Cc]:\\d+[dDwWmMyY])?'
rec.dates <- str_extract(todo.raw, rec.ptrn)
tasks <- cbind(prefix.map, data.frame(due = due.dates, threshold = thr.dates, recurrence = rec.dates))

# Helper function to create an array of strings containing the list and tag 
# identifiers.
get.strings <- function(string, pattern) {
  map <- str_extract_all(string, pattern)
  res <- do.call('c',
                 lapply(map, 
                        function(mm) {
                          ifelse(length(mm) == 0, '', gsub(' ', '', 
                                                           paste0(mm, 
                                                                  collapse = ',')))
                        }))
  return(res)
}

# This gives a list that needs to be flattened down.
tags <- get.strings(todo.raw, '( \\+\\w+)')
lists <- get.strings(todo.raw, '( \\@\\w+)')

# This results in 'half a data.frame' that does have vectors/c()'s as row elements,
# but perhaps a list of lists makes more sense.
# FIXME: needs to become a function so that I can re-use it for the lists/projects.
tags <- do.call('c', 
               lapply(1:length(tag.map),
                      function(ii) {
                        res <- ifelse(length(tag.map[[ii]]) == 0, 
                                      '',
                                      gsub(' ', '', 
                                           paste0(tag.map[[ii]], collapse = ',')))
                      }))
subset(tags, grepl('+hhs', tags))

# FIXME: integreren in het grote data frame
