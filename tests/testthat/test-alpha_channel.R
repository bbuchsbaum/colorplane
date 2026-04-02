# Tests for alpha_channel methods

test_that("alpha_channel works for HexColorPlane (normalized)", {
  clr <- c("#FF0000FF", "#00FF0080", "#0000FF00")
  cp <- HexColorPlane(clr)

  result <- alpha_channel(cp)

  expect_true(is.numeric(result))
  expect_length(result, 3)
  expect_equal(result[1], 1)       # FF = 255 -> 1
  expect_equal(result[2], 128/255) # 80 = 128 -> 0.502
  expect_equal(result[3], 0)       # 00 = 0 -> 0
})

test_that("alpha_channel works for HexColorPlane (non-normalized)", {
  clr <- c("#FF0000FF", "#00FF0080", "#0000FF00")
  cp <- HexColorPlane(clr)

  result <- alpha_channel(cp, normalize = FALSE)

  expect_equal(result[1], 255)
  expect_equal(result[2], 128)
  expect_equal(result[3], 0)
})

test_that("alpha_channel works for RGBColorPlane (normalized)", {
  rgba <- matrix(c(255, 0, 0, 255,
                   0, 255, 0, 128,
                   0, 0, 255, 0), nrow = 3, byrow = TRUE)
  cp <- RGBColorPlane(rgba)

  result <- alpha_channel(cp)

  expect_length(result, 3)
  expect_equal(result[1], 1)
  expect_equal(result[2], 128/255)
  expect_equal(result[3], 0)
})

test_that("alpha_channel works for RGBColorPlane (non-normalized)", {
  rgba <- matrix(c(255, 0, 0, 255,
                   0, 255, 0, 128), nrow = 2, byrow = TRUE)
  cp <- RGBColorPlane(rgba)

  result <- alpha_channel(cp, normalize = FALSE)

  expect_equal(result[1], 255)
  expect_equal(result[2], 128)
})

test_that("alpha_channel works for ConstantColorPlane", {
  cp <- ConstantColorPlane("#FF000080")

  result <- alpha_channel(cp)

  expect_equal(unname(result), 128/255)
})

test_that("alpha_channel for ConstantColorPlane (non-normalized)", {
  cp <- ConstantColorPlane("#FF0000FF")

  result <- alpha_channel(cp, normalize = FALSE)

  expect_equal(unname(result), 255)
})

test_that("alpha_channel after map_colors", {
  intensity <- seq(1, 5)
  cols <- rainbow(25)
  cp <- IntensityColorPlane(intensity, cols = cols)
  cl <- map_colors(cp, irange = c(0, 50))

  result <- alpha_channel(cl)

  expect_length(result, 5)
})
