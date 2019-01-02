context("Test contexts parser")

test_that("contexts are parsed correctly", {
  todo1 <- '@context xxx'
  res1 <- character()

  todo2 <- ' @context xxx'
  res2 <- '@context'

  todo3 <- 'xxx @context2 yyy @context3 zzz'
  res3 <- c('@context2', '@context3')

  expect_identical(parse_tags(todo1, '@'), res1)
  expect_identical(parse_tags(todo2, '@'), res2)
  expect_identical(parse_tags(todo3, '@'), res3)
})


context("Test projects parser")

test_that('projects are parsed correctly', {
  todo1 <- '+proj xxx'
  res1 <- tibble::tibble(context = character(), start = integer(), end = integer())

  todo2 <- ' +proj xxx'
  res2 <- tibble::tribble(
    ~context,    ~start, ~end,
    '+proj',  1L,     6L
  )

  todo3 <- 'xxx +proj2 yyy +proj3 zzz'
  res3 <- tibble::tribble(
    ~context,    ~start, ~end,
    '+proj2', 4L,     10L,
    '+proj3', 15L,    21L
  )

  expect_identical(parse_tags(todo1, '+'), res1)
  expect_identical(parse_tags(todo2, '+'), res2)
  expect_identical(parse_tags(todo3, '+'), res3)
})


context('Test due date parser')

test_that('due dates are parsed correctly', {
  todo <- c('xxx due:2018-12-32',
            'yyy due:2018-12-12 t:2018-12-10 @work',
            'xxx due:2018-12-31',
            'yyy due:2018-12-05, due:2018-12-29')
  expected <- lubridate::ymd(c(NA, '2018-12-12',
                               '2018-12-31', '2018-12-05'))

  expect_equal(parse_date(todo[1]), expected[1])
  expect_equal(parse_date(todo[2]), expected[2])
  expect_equal(parse_date(todo[3]), expected[3])
  expect_equal(parse_date(todo[4]), expected[4])
  expect_equal(parse_date(todo), expected)
})


context('Test threshold date parser')

test_that('threshold dates are parsed correctly', {
  todo <- c('xxx t:2018-12-32',
            'yyy due:2018-12-12 t:2018-12-10 @work',
            't:2018-12-31',
            ' t:2018-12-31',
            'yyy t:2018-12-05, due:2018-12-29')
  expected <- lubridate::ymd(c(NA, '2018-12-10', NA,
                               '2018-12-31', '2018-12-05'))

  expect_equal(parse_date(todo[1], 't'), expected[1])
  expect_equal(parse_date(todo[2], 't'), expected[2])
  expect_equal(parse_date(todo[3], 't'), expected[3])
  expect_equal(parse_date(todo[4], 't'), expected[4])
  expect_equal(parse_date(todo[5], 't'), expected[5])
  expect_equal(parse_date(todo, 't'), expected)
})


context('Test recurrence parser')

test_that('recurrences are recognized correctly', {
  todo <- c('rec:1m',
            ' rec:1w',
            ' rec:+1w',
            'xxx @context rec:2d yyy',
            'xxx rec:Ad',
            'xxx rec:2k')
  expected <- c(NA, '1w', '+1w', '2d', NA, NA)

  expect_equal(parse_recurrence(todo[1]), expected[1])
  expect_equal(parse_recurrence(todo[2]), expected[2])
  expect_equal(parse_recurrence(todo[3]), expected[3])
  expect_equal(parse_recurrence(todo[4]), expected[4])
  expect_equal(parse_recurrence(todo[5]), expected[5])
  expect_equal(parse_recurrence(todo[6]), expected[6])
  expect_equal(parse_recurrence(todo), expected)
})


context('Test parsing of prefix(es)')

test_that('prefixes are parsed correctly', {
  todo <- c('x 2018-12-12 2018-12-10 x y z',
            '(A) 2018-12-10 a b c',
            '2018-12-12 def',
            'a b c @context')
  expected <- structure(list(task = c("x 2018-12-12 2018-12-10 x y z",
                                      "(A) 2018-12-10 a b c",
                                      "2018-12-12 def", "a b c @context"),
                             done = c("x ", NA, NA, NA),
                             priority = c(NA, "(A) ", NA, NA),
                             date_completed = c("2018-12-12 ", NA, NA, NA),
                             date_created = c("2018-12-10 ", "2018-12-10 ",
                                              "2018-12-12 ", NA),
                             task_cleaned = c("x y z", "a b c", "def",
                                              "a b c @context")),
                        .Names = c("task", "done", "priority", "date_completed",
                                   "date_created", "task_cleaned"),
                        class = c("tbl_df", "tbl", "data.frame"),
                        row.names = c(NA, -4L))

  expect_identical(parse_prefixes(todo[1]), expected[1, , drop = FALSE])
  expect_identical(parse_prefixes(todo[2]), expected[2, , drop = FALSE])
  expect_identical(parse_prefixes(todo[3]), expected[3, , drop = FALSE])
  expect_identical(parse_prefixes(todo[4]), expected[4, , drop = FALSE])
  expect_identical(parse_prefixes(todo), expected)
})
