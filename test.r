setwd('c:/nlv19419/R/todoRtxt')
todo.dir <- 'C:/Users/NLV19419/Google Drive/todo'
library(stringr)

todo.raw <- readLines(con = file.path(todo.dir, 'todo.txt'), encoding = 'UTF-8') 
done.raw <- readLines(con = file.path(todo.dir, 'done.txt'), encoding = 'UTF-8') 

prefix.map <- str_match(todo.raw, '(^x +)?(\\([A-Z]\\) +)*(\\d{4}-\\d{2}-\\d{2} +)?(\\d{4}-\\d{2}-\\d{2} +)?(\\d{4}-\\d{2}-\\d{2} +)*(.*)')
proj.map <- str_match(remain, '( \\+\\w+).*\\1*')
post.infix.map <- str_match(todo.raw, '(( \\+\\w+).*\\1*)|( due:\\d{4}-\\d{2}-\\d{2})|( t:\\d{4}-\\d{2}-\\d{2})|( rec:\\d+[dwmy])|(( @\\w+).*\\1*)')

lent <- length(todo.raw)
lend <- length(done.raw)

todo <- do.call('rbind', 
                lapply(1:lent, 
                       function(ll) {
# options for the start of the line:
# - '... ' : task, no date-added
# - 'x ...' : completed 
                         prefix.map <- str_match(todo.raw, 
                                                 '(^x +)?(\\([A-Z]\\) +)*(\\d{4}-\\d{2}-\\d{2} +)?(\\d{4}-\\d{2}-\\d{2} +)?(\\d{4}-\\d{2}-\\d{2} +)*(.*)')
                         done <- prefix.map[, 3]
                         d.done <- prefix.map[, 4]
                         d.added <- prefix.map[, 5]
                         # ignore the other dates in column 6.
                         remain <- prefix.map[, 7]
                         browser()

                         
                         cat(done, '\t', d.done, '\t', d.added, '\t', substr(remain, 1, 10), '\n')
                         flush.console()
#                         thresh.pattern <- str_extract(todo.raw[ll], 
#                                                       't:[0-9]{4}-[0-9]{2}-[0-9]{2}')
#                         threshold <- as.Date(gsub('t:', '', thresh.pattern))
#                         remain <- str_extract(todo.raw[ll], 
#                                               '(?! t:[0-9]{4}-[0-9]{2}-[0-9]{2})')
#                         cat(threshold, '\n-->', remain, '\n')
#                         flush.console()
#                         d <- data.frame(threshold = threshold)
                         d <- data.frame(t.done = done)
                       }
                      )
                )
