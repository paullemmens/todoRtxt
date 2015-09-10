print <- function(x) UseMethod('print', x)

print.default <- function(x) return(x)

print.todoRtxt <- function(x, ...) {
  
# Figure out how to pad shorter task IDs.
  padding = nchar(max(x$idx))

# Complicated chain of %>% that I got from my question on SO:
# http://stackoverflow.com/questions/32416948/reverse-ordering-dplyrs-group-by
  x %>% arrange(idx) %>% group_by(threshold) %>%
    summarize(cat.task = paste0(threshold %>% first %>% format('%d-%m-%Y'), '\n',
                                '==========\n',
                                paste0(stringr::str_pad(idx, width = padding, side = 'left'), ':  ',
                                       orig.task) %>% unique %>% 
                                  paste(collapse = '\n'), 
                                '\n')) %>%
#    arrange(desc(threshold)) %>%
    arrange(desc(threshold %>% order(na.last = TRUE, decreasing = FALSE))) %>%
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
