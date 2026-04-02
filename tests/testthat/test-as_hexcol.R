# Tests for as_hexcol methods

test_that("as_hexcol works for RGBColorPlane with 4 columns", {
  rgba <- matrix(c(255, 0, 0, 255,
                   0, 255, 0, 128,
                   0, 0, 255, 0), nrow = 3, byrow = TRUE)
  cp <- RGBColorPlane(rgba)

  result <- as_hexcol(cp)

  expect_true(is.character(result))
  expect_length(result, 3)
  expect_equal(result[1], "#FF0000FF")
  expect_equal(result[2], "#00FF0080")
  expect_equal(result[3], "#0000FF00")
})

test_that("as_hexcol works for HexColorPlane (returns self)", {
  clr <- c("#FF0000FF", "#00FF00FF")
  cp <- HexColorPlane(clr)

  result <- as_hexcol(cp)

  expect_equal(result, clr)
})

test_that("as_hexcol round trips with as_rgb", {
  clr <- c("#FF0000FF", "#00FF00FF", "#0000FFFF")
  cp <- HexColorPlane(clr)

  rgb_result <- as_rgb(cp)
  rgb_cp <- RGBColorPlane(rgb_result)
  hex_result <- as_hexcol(rgb_cp)

  expect_equal(hex_result, clr)
})

test_that("as_hexcol preserves alpha", {
  rgba <- matrix(c(255, 0, 0, 128), nrow = 1)
  cp <- RGBColorPlane(rgba)

  result <- as_hexcol(cp)

  expect_true(grepl("80$", result))  # Should end in 80 (hex for 128)
})
