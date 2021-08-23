context('convert')

test_that("gregorianToEthiopian works", {
  expect_equal(gregorianToEthiopian(1982, 11, 21), '1975-03-12')
  expect_equal(gregorianToEthiopian(1941, 12, 7), '1934-03-28')
  expect_equal(gregorianToEthiopian(2010, 12, 22), '2003-04-13')
})

test_that("toEthiopian works", {
  expect_equal(toEthiopian('1982-11-21'), lubridate::as_date('1975-3-12'))
  expect_equal(toEthiopian('1941-12-7'), lubridate::as_date('1934-3-28'))
  expect_equal(toEthiopian('2010-12-22'), lubridate::as_date('2003-4-13'))

  expect_equal(toEthiopian(lubridate::as_date('1982-11-21')), lubridate::as_date('1975-3-12'))
  expect_equal(toEthiopian(lubridate::as_date('1941-12-7')), lubridate::as_date('1934-3-28'))
  expect_equal(toEthiopian(lubridate::as_date('2010-12-22')), lubridate::as_date('2003-4-13'))

  expect_equal(toEthiopian(c('1982-11-21','1941-12-7','2010-12-22')), lubridate::as_date(c('1975-03-12','1934-03-28','2003-04-13')))
})

test_that("ethiopianToGregorian works", {
  expect_equal(ethiopianToGregorian(2003, 4, 11), '2010-12-20')
  expect_equal(ethiopianToGregorian(1975, 3, 12), '1982-11-21')
})

test_that("toGregorian works", {
  expect_equal(toGregorian('2003-4-11'), lubridate::as_date('2010-12-20'))
  expect_equal(toGregorian('1975-3-12'), lubridate::as_date('1982-11-21'))

  expect_equal(toGregorian(lubridate::as_date('2003-4-11')), lubridate::as_date('2010-12-20'))
  expect_equal(toGregorian(lubridate::as_date('1975-3-12')), lubridate::as_date('1982-11-21'))

  expect_equal(toGregorian(c('2003-4-11','1975-3-12')), lubridate::as_date(c('2010-12-20','1982-11-21')))
})
