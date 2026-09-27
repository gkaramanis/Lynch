test_that("every palette has five valid hex colours", {
  for (pal in lynch_palettes) {
    expect_length(pal, 5)
    expect_match(pal, "^#[0-9A-Fa-f]{6}$")
  }
})

test_that("lynch_palette returns the whole palette by default", {
  pal <- lynch_palette("twin_peaks")
  expect_s3_class(pal, "palette")
  expect_equal(unclass(pal), lynch_palettes$twin_peaks, ignore_attr = TRUE)
})

test_that("lynch_palette returns the first n colours", {
  expect_length(lynch_palette("dune", 3), 3)
})

test_that("lynch_palette rejects unknown names and too many colours", {
  expect_error(lynch_palette("eraserhead"), "not found")
  expect_error(lynch_palette("dune", 6), "greater than")
})

test_that("the old mullholland_drive spelling still works, with a warning", {
  expect_warning(pal <- lynch_palette("mullholland_drive"), "deprecated")
  expect_equal(unclass(pal), lynch_palettes$mulholland_drive, ignore_attr = TRUE)
})
