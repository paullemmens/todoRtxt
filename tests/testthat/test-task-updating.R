context("Modify task properties")

tasks <- c('2018-12-12 my new task +tag due:2019-01-12 @work',
           'another new task @work due:2019-01-11',
           'due:2019-01-14 what +tag i need to do +job')

test_that("property modification works", {

  expected <- c('2018-12-12 my new task +tag due:2020-12-31 @work',
                'another new task @work due:2020-12-31',
                'due:2020-12-31 what +tag i need to do +job')

  expect_equal(modify_property(tasks[1], value = 'due:2020-12-31'), expected[1])
  expect_equal(modify_property(tasks[1], prop = 'due:', value = '2020-12-31'), expected[1])
  expect_equal(modify_property(tasks[1], prop = 'due', value = '2020-12-31'), expected[1])
  expect_equal(modify_property(tasks, value = 'due:2020-12-31'), expected)
})

})
