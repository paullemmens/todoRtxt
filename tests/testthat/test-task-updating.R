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
  ## expect_equal(modify_property(tasks[1], prop = 'due:', value = ''), expected[4])
})


context("Modify task tags")

tag_expected <- c('2018-12-12 my new task +job due:2019-01-12 @work',
                  'another new task @home due:2019-01-11',
                  'due:2019-01-14 what +tag i need to do +job')

test_that('tag modification works', {
  expect_equal(modify_tag(tasks[1], old = '+tag', new = '+job'), tag_expected[1])
  expect_equal(modify_tag(tasks[2], old = '@work', new = '@home'), tag_expected[2])
})

test_that('tag addition works', {
  expect_equal(modify_tag(tasks[3], old = '+tag', new = '+job +task'),
               'due:2019-01-14 what +job +task i need to do +job')
  expect_equal(modify_tag(tasks[3], old = '$', new = ' @work'),
               'due:2019-01-14 what +tag i need to do +job @work')
})

test_that('removing a tag works', {
  expect_equal(modify_tag(tasks[3], old = '+tag', new = ''),
               'due:2019-01-14 what  i need to do +job')
})


context("Modify task priority")

tasks <- c('(A) my high priority task @work due:2019-01-17 +mup',
           '(Z) low priority',
           'no priority')

expected <- c('(B) my high priority task @work due:2019-01-17 +mup',
              'low priority',
              '(C) no priority')

test_that('Updating existing priority works', {
  expect_equal(modify_priority(tasks[1], '(B)'), expected[1])
  expect_equal(modify_priority(tasks[1], '(B) '), expected[1])
})

test_that('Removing priority works', {
  expect_equal(modify_priority(tasks[2], ''), expected[2])
  expect_equal(modify_priority(tasks[3], ''), tasks[3])
})

test_that('Adding priority works', {
  expect_equal(modify_priority(tasks[3], '(C)'), expected[3])
  expect_equal(modify_priority(tasks[3], '(C) '), expected[3])
})

