# Changelog

## Lynch (development version)

- [`print()`](https://rdrr.io/r/base/print.html) on a palette has a
  cleaner look, with gaps between swatches and hex codes underneath, and
  returns the palette invisibly.
- [`scale_colour_lynch_c()`](https://gkaramanis.github.io/Lynch/reference/scale_colour_lynch_d.md)
  and
  [`scale_fill_lynch_c()`](https://gkaramanis.github.io/Lynch/reference/scale_colour_lynch_d.md)
  take a `guide` argument. Passing `guide` used to fail with “matched by
  multiple actual arguments”.
- New vignette,
  [`vignette("Lynch")`](https://gkaramanis.github.io/Lynch/articles/Lynch.md),
  and a package help page,
  [`?Lynch`](https://gkaramanis.github.io/Lynch/reference/Lynch-package.md).
- More examples in the help pages.

## Lynch 0.4.0

- New ggplot2 scales:
  [`scale_colour_lynch_d()`](https://gkaramanis.github.io/Lynch/reference/scale_colour_lynch_d.md)
  and
  [`scale_fill_lynch_d()`](https://gkaramanis.github.io/Lynch/reference/scale_colour_lynch_d.md)
  for categories,
  [`scale_colour_lynch_c()`](https://gkaramanis.github.io/Lynch/reference/scale_colour_lynch_d.md)
  and
  [`scale_fill_lynch_c()`](https://gkaramanis.github.io/Lynch/reference/scale_colour_lynch_d.md)
  for numbers, with sequential or diverging ramps and an optional
  `midpoint`. `scale_color_*` aliases included.

## Lynch 0.3.0

- Colour-blind-safe variants of five palettes: `blue_velvet_safe`,
  `dune_safe`, `mulholland_drive_safe`, `twin_peaks_safe` and
  `wild_at_heart_safe`. Each keeps the three or four colours of the
  film’s palette that stay distinct with red-green colour blindness.
  `lost_highway` already passes as it is.
- New sequential ramps (`lynch_sequential`, one per film) and diverging
  ramps (`lynch_diverging`, for four films).
  [`lynch_palette()`](https://gkaramanis.github.io/Lynch/reference/lynch_palette.md)
  gains `type = "sequential"` and `type = "diverging"` and interpolates
  ramps to any `n`.
- New `lynch_accessibility`: colour-blindness test results for every
  discrete palette.

## Lynch 0.2.0

- The Mulholland Drive palette is now `mulholland_drive`. The old
  spelling, `mullholland_drive`, still works with a deprecation warning.
- Added tests.
