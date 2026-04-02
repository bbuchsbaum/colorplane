# Tests for as_rgb methods

test_that("as_rgb works for RGBColorPlane", {
  rgba <- matrix(c(255, 0, 0, 255,
                   0, 255, 0, 128), nrow = 2, byrow = TRUE)
  cp <- RGBColorPlane(rgba)

  result <- as_rgb(cp)

  expect_true(is.matrix(result))
  expect_equal(result, rgba)
})

test_that("as_rgb works for HexColorPlane", {
  clr <- c("#FF0000FF", "#00FF00FF", "#0000FFFF")
  cp <- HexColorPlane(clr)

  result <- as_rgb(cp)

  expect_true(is.matrix(result))
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 4)

  # Check first color is red (use unname to strip column names)
  expect_equal(unname(result[1, 1]), 255)  # red
  expect_equal(unname(result[1, 2]), 0)    # green
  expect_equal(unname(result[1, 3]), 0)    # blue
  expect_equal(unname(result[1, 4]), 255)  # alpha
})

test_that("as_rgb works for ConstantColorPlane", {
  cp <- ConstantColorPlane("#00FF00FF")

  result <- as_rgb(cp)

  expect_true(is.matrix(result))
  expect_equal(unname(result[1, 1]), 0)    # red
  expect_equal(unname(result[1, 2]), 255)  # green
  expect_equal(unname(result[1, 3]), 0)    # blue
  expect_equal(unname(result[1, 4]), 255)  # alpha
})

test_that("as_rgb handles alpha channel", {
  clr <- c("#FF000080", "#00FF00FF")  # 50% alpha, full alpha
  cp <- HexColorPlane(clr)

  result <- as_rgb(cp)

  expect_equal(unname(result[1, 4]), 128)  # 50% alpha
  expect_equal(unname(result[2, 4]), 255)  # full alpha
})

test_that("as_rgb after map_colors works", {
  intensity <- 1:5
  cp <- IntensityColorPlane(intensity, cols = rainbow(5))
  mapped <- map_colors(cp)

  result <- as_rgb(mapped)

  expect_true(is.matrix(result))
  expect_equal(nrow(result), 5)
  expect_equal(ncol(result), 4)
})
