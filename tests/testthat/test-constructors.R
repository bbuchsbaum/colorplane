# Tests for constructor functions

test_that("IntensityColorPlane constructor works", {
  intensity <- 1:10
  cp <- IntensityColorPlane(intensity)

  expect_s4_class(cp, "IntensityColorPlane")
  expect_equal(cp@intensity, intensity)
  expect_equal(cp@alpha, 1)
  expect_length(cp@colmap, 255)
})

test_that("IntensityColorPlane with custom colors works", {
  intensity <- 1:5
  cols <- rainbow(10)
  cp <- IntensityColorPlane(intensity, cols = cols)

  expect_s4_class(cp, "IntensityColorPlane")
  expect_equal(cp@colmap, cols)
})

test_that("IntensityColorPlane with custom alpha works", {
  intensity <- 1:5
  cp <- IntensityColorPlane(intensity, alpha = 0.5)

  expect_equal(cp@alpha, 0.5)
})

test_that("IntensityColorPlane validates alpha input", {
  intensity <- 1:3
  cp <- IntensityColorPlane(intensity, alpha = c(0.2, 0.5, 0.8))

  expect_equal(cp@alpha, c(0.2, 0.5, 0.8))
  expect_error(IntensityColorPlane(intensity, alpha = c(0.2, 0.5)), "length 1 or 3")
  expect_error(IntensityColorPlane(intensity, alpha = 1.5), "between 0 and 1")
})

test_that("DiscreteColorPlane constructor works", {
  lookup <- list(a = "#FF0000FF", b = "#00FF00FF", c = "#0000FFFF")
  cp <- DiscreteColorPlane(lookup)

  expect_s4_class(cp, "DiscreteColorPlane")
  expect_equal(cp@lookup, lookup)
})

test_that("DiscreteColorPlane requires named list", {
  expect_error(DiscreteColorPlane(c("#FF0000", "#00FF00")), "must be a list")
  expect_error(DiscreteColorPlane(list("#FF0000", "#00FF00")), "must be a named list")
})

test_that("RGBColorPlane constructor works with 4 columns", {
  rgba <- matrix(c(255, 0, 0, 255,
                   0, 255, 0, 255,
                   0, 0, 255, 128), nrow = 3, byrow = TRUE)
  cp <- RGBColorPlane(rgba)

  expect_s4_class(cp, "RGBColorPlane")
  expect_equal(nrow(cp@clr), 3)
  expect_equal(ncol(cp@clr), 4)
})

test_that("RGBColorPlane constructor adds alpha for 3 columns", {
  rgb <- matrix(c(255, 0, 0,
                  0, 255, 0,
                  0, 0, 255), nrow = 3, byrow = TRUE)
  cp <- RGBColorPlane(rgb)

  expect_s4_class(cp, "RGBColorPlane")
  expect_equal(ncol(cp@clr), 4)
  expect_true(all(cp@clr[, 4] == 255))
})

test_that("RGBColorPlane rejects invalid input", {
  expect_error(RGBColorPlane(c(1, 2, 3)), "is.matrix")

  mat5 <- matrix(1:10, nrow = 2)
  expect_error(RGBColorPlane(mat5), "must be 4")
})

test_that("ConstantColorPlane constructor works", {
  cp <- ConstantColorPlane("#FF0000FF")

  expect_s4_class(cp, "ConstantColorPlane")
  expect_equal(cp@clr, "#FF0000FF")
})

test_that("ConstantColorPlane rejects invalid input", {
  expect_error(ConstantColorPlane(255), "is.character")
  expect_error(ConstantColorPlane(c("#FF0000", "#00FF00")), "length")
})

test_that("HexColorPlane constructor works", {
  clr <- c("#FF0000FF", "#00FF00FF", "#0000FFFF")
  cp <- HexColorPlane(clr)

  expect_s4_class(cp, "HexColorPlane")
  expect_equal(cp@clr, clr)
  expect_length(cp@clr, 3)
})

test_that("HexColorPlane rejects non-character input", {
  expect_error(HexColorPlane(c(1, 2, 3)), "is.character")
})
