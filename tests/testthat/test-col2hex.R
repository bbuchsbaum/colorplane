# Tests for col2hex function

test_that("col2hex converts single color", {
  result <- col2hex("red")

  expect_true(is.character(result))
  expect_length(result, 1)
  expect_true(grepl("^#", result))
})

test_that("col2hex converts multiple colors", {
  result <- col2hex(c("red", "green", "blue"))

  expect_length(result, 3)
  expect_true(all(grepl("^#", result)))
})

test_that("col2hex respects alpha parameter", {
  result_full <- col2hex("red", alpha = 1)
  result_half <- col2hex("red", alpha = 0.5)
  result_zero <- col2hex("red", alpha = 0)

  # Full alpha should end with FF
  expect_true(grepl("FF$", result_full))

  # Zero alpha should end with 00
  expect_true(grepl("00$", result_zero))

  # Half alpha should end with 80 (approximately)
  expect_true(grepl("80$", result_half))
})

test_that("col2hex works with named colors", {
  colors <- c("red", "blue", "green", "yellow", "white", "black")
  result <- col2hex(colors)

  expect_length(result, 6)
  expect_true(all(nchar(result) == 9))  # #RRGGBBAA format
})

test_that("col2hex alpha applies to all colors", {
  result <- col2hex(c("red", "blue"), alpha = 0.5)

  # Both should have same alpha
  expect_true(grepl("80$", result[1]))
  expect_true(grepl("80$", result[2]))
})
