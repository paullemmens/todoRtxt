context('Test group_by')

tasks <- tibble::tribble(
  ~id, ~context, ~project,
  1,   '@home', '+project',
  2,   c('@home', '@work'), '+project',
  3,   c('@home', '@work'), c('+proj1', '+proj2'),
  4,   c('@home', '@work'), NA_character_,
  5,   NA_character_, '+project')

test_that('group_by_context works', {
  res <- structure(list(id = c(1, 2, 2, 3, 3, 4, 4, 5),
                        context = c("@home", "@home", "@work", "@home", "@work",
                                    "@home", "@work", NA),
                        project = list("+project", "+project", "+project",
                                       c("+proj1", "+proj2"), c("+proj1", "+proj2"),
                                       NA_character_, NA_character_, "+project")),
                   class = c("grouped_df", "tbl_df", "tbl", "data.frame"),
                   row.names = c(NA, -8L),
                   groups = structure(list(context = c("@home", "@work", NA),
                                           .rows = structure(list(c(1L, 2L, 4L, 6L),
                                                                  c(3L, 5L, 7L), 8L),
                                                             ptype = integer(0),
                                                             class = c("vctrs_list_of", "vctrs_vctr", "list"))),
                                      class = c("tbl_df", "tbl", "data.frame"),
                                      row.names = c(NA, -3L),
                                      .drop = TRUE))

  expect_identical(group_by_context(tasks), res)
})


test_that('group_by_project works', {
    res <- structure(list(id = c(1, 2, 3, 3, 4, 5),
                          context = list("@home", c("@home", "@work"),
                                         c("@home", "@work"), c("@home", "@work"),
                                         c("@home", "@work"), NA_character_),
                          project = c("+project", "+project", "+proj1", "+proj2",
                                      NA, "+project")),
                     class = c("grouped_df", "tbl_df", "tbl", "data.frame"),
                     row.names = c(NA, -6L),
                     groups = structure(list(project = c("+proj1", "+proj2", "+project", NA),
                                             .rows = structure(list(3L, 4L, c(1L, 2L, 6L), 5L),
                                                               ptype = integer(0),
                                                               class = c("vctrs_list_of", "vctrs_vctr", "list"))),
                                        class = c("tbl_df", "tbl", "data.frame"),
                                        row.names = c(NA, -4L),
                                        .drop = TRUE))

  expect_identical(group_by_project(tasks), res)
})


test_that('Grouping by context and project works', {
  res <- structure(list(id = c(1, 2, 2, 3, 3, 3, 3, 4, 4, 5),
                        context = c("@home", "@home", "@work", "@home", "@home",
                                    "@work", "@work", "@home", "@work", NA),
                        project = c("+project", "+project", "+project", "+proj1",
                                    "+proj2", "+proj1", "+proj2", NA, NA, "+project")),
                   class = c("grouped_df", "tbl_df", "tbl", "data.frame"),
                   row.names = c(NA, -10L),
                   groups = structure(list(project = c("+proj1", "+proj2", "+project", NA),
                                           .rows = structure(list(c(4L, 6L), c(5L, 7L),
                                                                  c(1L, 2L, 3L, 10L), 8:9),
                                                             ptype = integer(0),
                                                             class = c("vctrs_list_of", "vctrs_vctr", "list"))),
                                      class = c("tbl_df", "tbl", "data.frame"),
                                      row.names = c(NA, -4L),
                                      .drop = TRUE))
  expect_identical(group_by_context(tasks) %>% group_by_project(), res)
})
