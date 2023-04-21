context('Test filtering')

test_that('tags are found', {
  todo1 <- list()
  res1 <- NA_character_
  expect_identical(extract_tags(todo1, '@'), res1)
  expect_identical(extract_tags(todo1, 'context'), res1)
  expect_identical(extract_tags(todo1, 'list'), res1)

  todo2a <- list('@home')
  todo2b <- list('+project')
  res2a <- '@home'
  res2b <- '+project'
  expect_identical(extract_tags(todo2a, '@'), res2a)
  expect_identical(extract_tags(todo2a, '+'), res1) # Intentionally res1.
  expect_identical(extract_tags(todo2b, '@'), res1) # Intentionally res1.
  expect_identical(extract_tags(todo2b, '+'), res2b)

  todo3 <- list('@home', c('@home', '@work'), '+list', '', NA,
                c('@home', '+project'), c('+list', '+project'))
  res3a <- c('@home', '@work')
  res3b <- c('+list', '+project')
  expect_identical(extract_tags(todo3, '@'), res3a)
  expect_identical(extract_tags(todo3, '+'), res3b)
  expect_identical(extract_tags(todo3, 'project'), res3b)
})

test_that('tags can be listed', {
  todo <- list('@home', c('@home', '@work'), '+list', '', NA,
               c('@home', '+project'), c('+list', '+project'))
  res1 <- c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)
  res2 <- c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE)
  res3 <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)

  expect_identical(find_tag(todo, '@home'), res1)
  expect_identical(find_tag(todo, '+list'), res2)
  expect_identical(find_tag(todo, '+project'), res3)
})

test_that('filtering works', {
  todo <- c('x 2018-12-12 2018-12-10 x y z @home +list',
            '(A) 2018-12-10 a b c @home @work',
            '2018-12-12 def +list',
            'a b c @context +list +project')
  res1 <- structure(list(task = c("x 2018-12-12 2018-12-10 x y z @home +list",
"(A) 2018-12-10 a b c @home @work"), done = c("x ", NA), priority = c(NA,
"A"), date_completed = structure(c(17877, NA), class = "Date"),
    date_created = structure(c(17875, 17875), class = "Date"),
    task_cleaned = c("x y z @home +list", "a b c @home @work"
    ), date_due = structure(c(NA_real_, NA_real_), class = "Date"),
    date_threshold = structure(c(NA_real_, NA_real_), class = "Date"),
    recurrence = c(NA_character_, NA_character_), context = list(
        "@home", c("@home", "@work")), project = list("+list",
        character(0))), row.names = c(NA, -2L), class = c("todoRtxt",
"tbl_df", "tbl", "data.frame"))

  expect_identical(parse_tasks(todo) %>% dplyr::filter(find_tag(context, '@home')), res1)
})
