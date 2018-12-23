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
