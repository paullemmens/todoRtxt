setwd('c:/nlv19419/R/todoRtxt')
#todo.dir <- 'C:/Users/NLV19419/Google Drive/todo'
todo.dir <- 'C:/nlv19419/Box Sync/todo'
library(stringr)
library(dplyr)
library(magrittr)

source('file.loading.r')
todo.raw <- load.todo(path = todo.dir)
done.raw <- load.done(path = todo.dir)

source('todotxt.parsing.r')
tasks <- parse.todotxt(todo.raw)

source('print.todoRtxt.r')
tasks

source('list.todoRtxt.r')
t.list()
t.list('backlog')

source('view.todoRtxt.r')
tv()

subset(tags, grepl('+hhs', tags))


##
library(tidyverse)
devtools::load_all()
loc <- '~/nextcloud/todo/todo.txt'
tasks <- load_tasks(loc)
