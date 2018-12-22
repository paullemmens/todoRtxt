context("test-parse_contexts")

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

  expect_identical(parse_contexts(todo1), res1)
  expect_identical(parse_contexts(todo2), res2)
  expect_identical(parse_contexts(todo3), res3)
})
