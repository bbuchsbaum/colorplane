# Tests for map_colors methods

test_that("map_colors works for IntensityColorPlane", {
  intensity <- 1:10
  cols <- rainbow(10)
  cp <- IntensityColorPlane(intensity, cols = cols)

  result <- map_colors(cp)

  expect_s4_class(result, "HexColorPlane")
  expect_length(result@clr, 10)
})

test_that("map_colors works with irange parameter", {
  intensity <- seq(1, 100)
  cols <- rainbow(25)
  cp <- IntensityColorPlane(intensity, cols = cols)

  result <- map_colors(cp, irange = c(0, 50))

  expect_s4_class(result, "HexColorPlane")
  expect_equal(result@clr[50], rainbow(25)[25])
})

test_that("map_colors handles NA values in intensity", {
  intensity <- c(1, NA, 3, NA, 5)
  cp <- IntensityColorPlane(intensity)

  result <- map_colors(cp)

  expect_s4_class(result, "HexColorPlane")
  expect_equal(result@clr[2], "#00000000")
  expect_equal(result@clr[4], "#00000000")
})

test_that("map_colors handles all-missing intensity values without warnings", {
  cp <- IntensityColorPlane(as.numeric(c(NA, NA)), cols = rainbow(5))

  expect_no_warning(result <- map_colors(cp))
  expect_equal(result@clr, c("#00000000", "#00000000"))
})

test_that("map_colors handles constant intensity values deterministically", {
  cols <- c("#000000FF", "#111111FF", "#222222FF", "#333333FF", "#444444FF")
  cp <- IntensityColorPlane(rep(5, 3), cols = cols)

  result <- map_colors(cp)

  expect_equal(result@clr, rep(cols[3], 3))
})

test_that("map_colors handles zero-width irange by clipping to endpoints and midpoint", {
  cols <- c("#000000FF", "#111111FF", "#222222FF", "#333333FF", "#444444FF")
  cp <- IntensityColorPlane(c(4, 5, 6), cols = cols)

  result <- map_colors(cp, irange = c(5, 5))

  expect_equal(result@clr, c(cols[1], cols[3], cols[5]))
})

test_that("map_colors with threshold returns RGBColorPlane", {
  intensity <- 1:10
  cp <- IntensityColorPlane(intensity)

  result <- map_colors(cp, threshold = 5)

  expect_s4_class(result, "RGBColorPlane")
})

test_that("map_colors with single threshold makes values transparent", {
  intensity <- 1:10
  cp <- IntensityColorPlane(intensity)

  result <- map_colors(cp, threshold = 5)

  # Values below 5 should be transparent (alpha = 0)
  expect_true(all(result@clr[1:4, 4] == 0))
  # Values >= 5 should have alpha > 0
  expect_true(all(result@clr[5:10, 4] > 0))
})

test_that("map_colors with two-sided threshold works", {
  intensity <- -5:5
  cp <- IntensityColorPlane(intensity)

  result <- map_colors(cp, threshold = c(-2, 2))

  # Values > -2 AND < 2 should be transparent (indices 5,6,7 = values -1,0,1)
  expect_true(all(result@clr[5:7, 4] == 0))
  # Values at thresholds or beyond should NOT be transparent
  expect_true(result@clr[4, 4] > 0)   # -2 is NOT > -2, so NOT transparent
  expect_true(result@clr[8, 4] > 0)   # 2 is NOT < 2, so NOT transparent
})

test_that("map_colors threshold with alpha works", {
  intensity <- 1:10
  cp <- IntensityColorPlane(intensity)

  result <- map_colors(cp, alpha = 0.5, threshold = 5)

  expect_s4_class(result, "RGBColorPlane")
  # Non-transparent values should have reduced alpha
  expect_true(all(result@clr[5:10, 4] <= 255 * 0.5 + 1))  # +1 for rounding
})

test_that("map_colors applies alpha even without threshold", {
  cp <- IntensityColorPlane(1:2, cols = c("#FF0000FF", "#00FF00FF"))

  result <- map_colors(cp, alpha = 0.5)

  expect_s4_class(result, "HexColorPlane")
  expect_equal(alpha_channel(result, normalize = FALSE), c(127.5, 127.5), tolerance = 1)
})

test_that("map_colors rejects invalid alpha", {
  intensity <- 1:10
  cp <- IntensityColorPlane(intensity)

  expect_error(map_colors(cp, alpha = 1.5))
  expect_error(map_colors(cp, alpha = -0.1))
})

test_that("map_colors rejects invalid threshold length", {
  intensity <- 1:10
  cp <- IntensityColorPlane(intensity)

  expect_error(map_colors(cp, threshold = c(1, 2, 3)), "1 or 2 elements")
})

test_that("map_colors works for ConstantColorPlane", {
  cp <- ConstantColorPlane("#FF0000FF")

  result <- map_colors(cp)

  expect_s4_class(result, "HexColorPlane")
  expect_equal(result@clr, "#FF0000FF")
})

test_that("map_colors works for HexColorPlane (returns self)", {
  clr <- c("#FF0000FF", "#00FF00FF")
  cp <- HexColorPlane(clr)

  result <- map_colors(cp)

  expect_s4_class(result, "HexColorPlane")
  expect_identical(result, cp)
})

test_that("map_colors works for DiscreteColorPlane", {
  lookup <- list(a = "#FF0000FF", b = "#00FF00FF", c = "#0000FFFF")
  cp <- DiscreteColorPlane(lookup)

  result <- map_colors(cp, values = c("a", "b", "c", "a"))

  expect_s4_class(result, "HexColorPlane")
  expect_length(result@clr, 4)
  expect_equal(unname(result@clr[1]), "#FF0000FF")
  expect_equal(unname(result@clr[2]), "#00FF00FF")
  expect_equal(unname(result@clr[4]), "#FF0000FF")
})

test_that("map_colors handles missing values in DiscreteColorPlane", {
  lookup <- list(a = "#FF0000FF", b = "#00FF00FF")
  cp <- DiscreteColorPlane(lookup)

  result <- map_colors(cp, values = c("a", "x", "b"))

  expect_length(result@clr, 3)
  expect_equal(unname(result@clr[2]), "#00000000")  # Missing value gets transparent default
})

test_that("map_colors with irange clips correctly", {
  intensity <- 1:100
  cols <- rainbow(10)
  cp <- IntensityColorPlane(intensity, cols = cols)

  # Test with irange smaller than actual range
  result <- map_colors(cp, irange = c(25, 75))

  expect_s4_class(result, "HexColorPlane")
  # First color should be first in colormap (clipped to 1)
  expect_equal(result@clr[1], cols[1])
  # Last color should be last in colormap (clipped to max)
  expect_equal(result@clr[100], cols[10])
})

test_that("map_colors uses stored alpha from IntensityColorPlane", {
  intensity <- 1:10
  cp <- IntensityColorPlane(intensity, alpha = 0.5)

  # With threshold, alpha is applied

  result <- map_colors(cp, threshold = 5)

  expect_s4_class(result, "RGBColorPlane")
  # Non-transparent values should have reduced alpha (0.5 * 255 = 127.5)
  expect_true(all(result@clr[5:10, 4] <= 128))
})

test_that("map_colors preserves per-element alpha stored on IntensityColorPlane", {
  cp <- IntensityColorPlane(
    1:3,
    cols = c("#FF0000FF", "#00FF00FF", "#0000FFFF"),
    alpha = c(0.2, 0.5, 0.8)
  )

  result <- map_colors(cp)

  expect_equal(alpha_channel(result, normalize = FALSE), c(51, 128, 204), tolerance = 1)
})

test_that("map_colors alpha parameter overrides stored alpha", {
  intensity <- 1:10
  cp <- IntensityColorPlane(intensity, alpha = 0.5)

  # Explicit alpha=1 should override the stored 0.5
  result <- map_colors(cp, alpha = 1, threshold = 5)

  expect_s4_class(result, "RGBColorPlane")
  # Non-transparent values should have full alpha
  expect_true(all(result@clr[5:10, 4] == 255))
})
