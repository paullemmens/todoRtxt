context("Test contexts parser")

test_that("contexts are parsed correctly", {
  todo1 <- '@context xxx'
  res1 <- tibble::tibble(context = character(), start = integer(), end = integer())

  todo2 <- ' @context xxx'
  res2 <- tibble::tribble(
    ~context,    ~start, ~end,
    '@context',  1L,     9L
  )

  todo3 <- 'xxx @context2 yyy @context3 zzz'
  res3 <- tibble::tribble(
    ~context,    ~start, ~end,
    '@context2', 4L,     13L,
    '@context3', 18L,    27L
  )

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
})
