setwd('c:/nlv19419/R/todoRtxt')
todo.dir <- 'C:/Users/NLV19419/Google Drive/todo'
library(stringr)

todo.raw <- readLines(con = file.path(todo.dir, 'todo.txt'), encoding = 'UTF-8') 
done.raw <- readLines(con = file.path(todo.dir, 'done.txt'), encoding = 'UTF-8') 

# This file in the repo of Simpletask cloudless shows the patterns that author 
# uses to get thing working.
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


# FIXME: blijven steken bij het optimaliseren/goed maken van het dates.map patroon.
# in de oude situatie reageerde het maar op 1 van de 3 (sub)patronen in de expressie.
# Zoals het patroon hieronder staat, match het wel alles, maar alleen die items die alles hebben staan.
dates.map <- data.frame(str_match(todo.raw, '( [Dd][Uu][Ee]:\\d{4}-\\d{2}-\\d{2})?.*( [Tt]:\\d{4}-\\d{2}-\\d{2})?'), #?.*( [Rr][Ee][Cc]:\\d+[dDwWmMyY])?'),
                        stringsAsFactors = FALSE)
#names(dates.map) <- c()

# This gives a list that needs to be flattened down.
tag.map <- str_match_all(todo.raw , '( \\+\\w+)')
list.map <- str_match_all(todo.raw , '( \\@\\w+)')

# This results in 'half a data.frame' that does have vectors/c()'s as row elements,
# but perhaps a list of lists makes more sense.
# FIXME: needs to become a function so that I can re-use it for the lists/projects.
tags <- do.call('rbind', 
               lapply(1:length(tag.map),
                      function(rr) {
                        d <- data.frame(rr = rr)
                        if (length(tag.map[[rr]]) == 0) {
                          d$tags = '' #list(tag.map[[rr]])
                        } else if (dim(tag.map[[rr]])[1] == 1) {
                          d$tags <- gsub(' ', '', tag.map[[rr]][1, 1])
                        } else {
                          d$tags <- gsub(' ', '', paste0(tag.map[[rr]][, 1], 
                                                         collapse = ','))
                        } 
                        return(d)
                      }))
subset(tags, grepl('+hhs', tags))

# FIXME: integreren in het grote data frame
