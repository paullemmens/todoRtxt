context("test-task-updating")

test_that("property modification works", {
  tasks <- c('2018-12-12 my new task due:2019-01-12 @work',
             'another new task @work due:2019-01-11',
             'due:2019-01-14 what i need to do')
  new_value <- 'due:2020-12-31'
  expected <- c('2018-12-12 my new task due:2020-12-31 @work',
                'another new task @work due:2020-12-31',
                'due:2020-12-31 what i need to do')

  expect_equal(modify_task(tasks[1], value = new_value), expected[1])
  expect_equal(modify_task(tasks[1], prop = 'due', value = '2020-12-31'), expected[1])
  expect_equal(modify_task(tasks, value = new_value), expected)
})
