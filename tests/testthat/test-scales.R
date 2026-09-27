test_that("discrete scales use the palette's colours in order", {
  sc <- scale_fill_lynch_d("twin_peaks_safe")
  expect_s3_class(sc, "ScaleDiscrete")
  expect_equal(sc$palette(3), lynch_palettes$twin_peaks_safe)
  expect_equal(scale_colour_lynch_d("dune", direction = -1)$palette(5), rev(lynch_palettes$dune))
})

test_that("discrete scales warn when a palette runs out", {
  expect_warning(scale_fill_lynch_d("twin_peaks_safe")$palette(4), "shown as NA")
})

test_that("unknown palettes error", {
  expect_error(scale_fill_lynch_d("eraserhead"), "not found")
  expect_error(scale_fill_lynch_c("dune", type = "diverging"), "not found")
})

test_that("continuous scales run from the ramp's first to last colour", {
  sc <- scale_fill_lynch_c("dune")
  expect_s3_class(sc, "ScaleContinuous")
  ends <- sc$palette(c(0, 1))
  expect_lte(max(abs(col2rgb(ends) - col2rgb(lynch_sequential$dune[c(1, 7)]))), 2)
})

test_that("a midpoint puts the centre colour at that value", {
  sc <- scale_fill_lynch_c("mulholland_drive", type = "diverging", midpoint = 0)
  sc$train(c(-5, 15))
  expect_lte(max(abs(col2rgb(sc$map(0)) - col2rgb(lynch_diverging$mulholland_drive[5]))), 2)
})

test_that("scales work in a plot", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = hp)) +
    ggplot2::geom_point() +
    scale_colour_lynch_c("twin_peaks")
  expect_no_error(ggplot2::ggplot_build(p))
})
