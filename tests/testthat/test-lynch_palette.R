test_that("every palette holds valid hex colours", {
  for (pal in c(lynch_palettes, lynch_sequential, lynch_diverging)) {
    expect_match(pal, "^#[0-9A-Fa-f]{6}$")
  }
})

test_that("film palettes have five colours and safe ones three or four", {
  safe <- grepl("_safe$", names(lynch_palettes))
  expect_true(all(lengths(lynch_palettes[!safe]) == 5))
  expect_true(all(lengths(lynch_palettes[safe]) %in% 3:4))
})

test_that("safe palettes are subsets of their film palette", {
  for (name in grep("_safe$", names(lynch_palettes), value = TRUE)) {
    film <- sub("_safe$", "", name)
    expect_true(all(lynch_palettes[[name]] %in% lynch_palettes[[film]]))
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

test_that("ramps interpolate to any length", {
  expect_length(lynch_palette("dune", 12, type = "sequential"), 12)
  expect_length(lynch_palette("twin_peaks", type = "diverging"), 9)
})

test_that("lynch_palette rejects unknown names and too many colours", {
  expect_error(lynch_palette("eraserhead"), "not found")
  expect_error(lynch_palette("dune", 6), "greater than")
  expect_error(lynch_palette("dune", type = "diverging"), "not found")
})

test_that("the old mullholland_drive spelling still works, with a warning", {
  expect_warning(pal <- lynch_palette("mullholland_drive"), "deprecated")
  expect_equal(unclass(pal), lynch_palettes$mulholland_drive, ignore_attr = TRUE)
})

test_that("accessibility table covers every discrete palette", {
  expect_setequal(lynch_accessibility$palette, names(lynch_palettes))
  safe <- lynch_accessibility$palette[lynch_accessibility$colourblind_safe]
  expect_true(all(grepl("_safe$", setdiff(safe, "lost_highway"))))
})

test_that("printing a palette draws it and returns invisibly", {
  pdf(NULL)
  on.exit(dev.off())
  pal <- lynch_palette("dune")
  expect_identical(expect_invisible(print(pal)), pal)
})
