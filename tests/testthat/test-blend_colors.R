# Tests for blend_colors methods

test_that("blend_colors works for two ColorPlane objects", {
  top <- IntensityColorPlane(1:5, cols = rainbow(5))
  bottom <- IntensityColorPlane(1:5, cols = rev(rainbow(5)))

  top_mapped <- map_colors(top)
  bottom_mapped <- map_colors(bottom)

  result <- blend_colors(bottom_mapped, top_mapped, alpha = 0.5)

  expect_s4_class(result, "RGBColorPlane")
  expect_equal(nrow(result@clr), 5)
})

test_that("blend_colors with alpha=1 results in top visible", {
  # Create fully opaque top and bottom
  bottom_rgb <- matrix(c(255, 0, 0, 255), nrow = 1)  # red
  top_rgb <- matrix(c(0, 255, 0, 255), nrow = 1)     # green

  bottom <- RGBColorPlane(bottom_rgb)
  top <- RGBColorPlane(top_rgb)

  result <- blend_colors(bottom, top, alpha = 1)

  # With fully opaque top and alpha=1, should see mostly green
  expect_equal(result@clr[1, 2], 255)  # green component
})

test_that("blend_colors with alpha=0 results in bottom visible", {
  bottom_rgb <- matrix(c(255, 0, 0, 255), nrow = 1)  # red, opaque
  top_rgb <- matrix(c(0, 255, 0, 255), nrow = 1)     # green, opaque

  bottom <- RGBColorPlane(bottom_rgb)
  top <- RGBColorPlane(top_rgb)

  result <- blend_colors(bottom, top, alpha = 0)

  # With alpha=0, top is invisible so we see bottom
  expect_equal(result@clr[1, 1], 255)  # red component
  expect_equal(result@clr[1, 2], 0)    # green component
})

test_that("blend_colors with missing alpha uses default 1", {
  top <- IntensityColorPlane(1:3, cols = rainbow(3))
  bottom <- IntensityColorPlane(1:3, cols = rev(rainbow(3)))

  top_mapped <- map_colors(top)
  bottom_mapped <- map_colors(bottom)

  result <- blend_colors(bottom_mapped, top_mapped)

  expect_s4_class(result, "RGBColorPlane")
})

test_that("blend_colors rejects invalid alpha", {
  bottom <- RGBColorPlane(matrix(c(255, 0, 0, 255), nrow = 1))
  top <- RGBColorPlane(matrix(c(0, 255, 0, 255), nrow = 1))

  expect_error(blend_colors(bottom, top, alpha = 1.5))
  expect_error(blend_colors(bottom, top, alpha = -0.5))
})

test_that("blend_colors HexColorPlane with RGBColorPlane works", {
  bottom <- HexColorPlane(c("#FF0000FF", "#00FF00FF"))
  top_rgb <- matrix(c(0, 0, 255, 128,
                      255, 255, 0, 128), nrow = 2, byrow = TRUE)
  top <- RGBColorPlane(top_rgb)

  result <- blend_colors(bottom, top, alpha = 1)

  expect_s4_class(result, "RGBColorPlane")
  expect_equal(nrow(result@clr), 2)
})

test_that("blend_colors HexColorPlane with ConstantColorPlane works", {
  bottom <- HexColorPlane(c("#FF0000FF", "#00FF00FF", "#0000FFFF"))
  top <- ConstantColorPlane("#FFFFFF80")  # semi-transparent white

  result <- blend_colors(bottom, top, alpha = 1)

  expect_s4_class(result, "RGBColorPlane")
  expect_equal(nrow(result@clr), 3)
})

test_that("blend_colors produces correct alpha compositing", {
  # Test alpha compositing formula:
  # ao = αtop + αbottom * (1 - αtop)
  # Clr_out = (Clr_top * αtop + Clr_bottom * αbottom * (1 - αtop)) / ao

  # Semi-transparent top over opaque bottom
  bottom_rgb <- matrix(c(100, 100, 100, 255), nrow = 1)
  top_rgb <- matrix(c(200, 200, 200, 128), nrow = 1)  # ~50% alpha

  bottom <- RGBColorPlane(bottom_rgb)
  top <- RGBColorPlane(top_rgb)

  result <- blend_colors(bottom, top, alpha = 1)

  expect_s4_class(result, "RGBColorPlane")
  # Result should be somewhere between 100 and 200
  expect_true(result@clr[1, 1] > 100 && result@clr[1, 1] < 200)
})

test_that("blend_colors handles transparent top", {
  bottom_rgb <- matrix(c(255, 0, 0, 255), nrow = 1)  # opaque red
  top_rgb <- matrix(c(0, 255, 0, 0), nrow = 1)       # fully transparent green

  bottom <- RGBColorPlane(bottom_rgb)
  top <- RGBColorPlane(top_rgb)

  result <- blend_colors(bottom, top, alpha = 1)

  # With transparent top, should see only bottom
  expect_equal(result@clr[1, 1], 255)  # red
  expect_equal(result@clr[1, 2], 0)    # no green
})

test_that("blend_colors handles transparent bottom", {
  bottom_rgb <- matrix(c(255, 0, 0, 0), nrow = 1)    # fully transparent red
  top_rgb <- matrix(c(0, 255, 0, 255), nrow = 1)     # opaque green

  bottom <- RGBColorPlane(bottom_rgb)
  top <- RGBColorPlane(top_rgb)

  result <- blend_colors(bottom, top, alpha = 1)

  # With transparent bottom and opaque top, should see only top
  expect_equal(result@clr[1, 2], 255)  # green
})

test_that("blend_colors preserves composite alpha", {
  bottom <- RGBColorPlane(matrix(c(255, 0, 0, 128), nrow = 1))
  top <- RGBColorPlane(matrix(c(0, 255, 0, 128), nrow = 1))

  result <- blend_colors(bottom, top, alpha = 1)
  expected_alpha <- (128 / 255 + (128 / 255) * (1 - 128 / 255)) * 255

  expect_equal(alpha_channel(result, normalize = FALSE), expected_alpha, tolerance = 1e-6)
})

test_that("blend_colors keeps fully transparent blends finite", {
  bottom <- RGBColorPlane(matrix(c(255, 0, 0, 0), nrow = 1))
  top <- RGBColorPlane(matrix(c(0, 255, 0, 0), nrow = 1))

  result <- blend_colors(bottom, top, alpha = 1)

  expect_true(all(is.finite(result@clr)))
  expect_equal(unname(result@clr[1, 1:3]), c(0, 0, 0))
  expect_equal(alpha_channel(result, normalize = FALSE), 0)
})

test_that("blend_colors requires matching plane lengths", {
  bottom <- RGBColorPlane(matrix(c(
    255, 0, 0, 255,
    0, 255, 0, 255
  ), nrow = 2, byrow = TRUE))
  top <- RGBColorPlane(matrix(c(0, 0, 255, 255), nrow = 1))

  expect_error(blend_colors(bottom, top, alpha = 1), "same number of colors")
})
