# view past due
# view today
# view 

t.view <- function(x = tasks, completed = FALSE, reverse = TRUE, ...) {
  
# Figure out how to pad shorter task IDs.
  padding = nchar(max(x$idx))

# Complicated chain of %>% that I got from my question on SO:
# http://stackoverflow.com/questions/32416948/reverse-ordering-dplyrs-group-by
  d <- x %>% arrange(idx) 

# If completed tasks should not be displayed, filter only those that do not have 
# that flag.
  if (!completed) {
    d <- filter(d, is.na(done))
  }
  
# Create the string to cat() using a complicated dplyr chain of %>%.
  d <- d %>% group_by(threshold) %>%
    summarize(cat.task = paste0(threshold %>% first %>% format('%d-%m-%Y'), '\n',
                                '==========\n',
                                paste0(stringr::str_pad(idx, width = padding, side = 'left'), ':  ',
                                       orig.task) %>% unique %>% 
                                  paste(collapse = '\n'), 
                                '\n')) 
  
# Because task lists can be long and top items (printed first) may scroll of 
# the screen but are those having top priority (because printed first implies
# threshold date is closest to today). Therefore, by default reverse the list
# so that they end up closest to the command prompt.
  if (reverse) {
    d <- d %>% arrange(desc(threshold %>% order(na.last = TRUE, decreasing = FALSE))) 
  } else {
    d <- d %>% arrange(desc(threshold %>% order(na.last = TRUE, decreasing = TRUE))) 
  }

  d %>% use_series(cat.task) %>% cat(fill = TRUE, sep = '')

# Wrap up
  return(invisible())
}

tv <- function(x, ...) t.view(x, ...)
