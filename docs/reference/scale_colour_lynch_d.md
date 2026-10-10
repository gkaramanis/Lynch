# Lynch scales for ggplot2

Colour and fill scales using the Lynch palettes. The `_d` scales are for
categories and use a discrete palette from
[lynch_palettes](https://gkaramanis.github.io/Lynch/reference/lynch_palettes.md);
the `_c` scales are for numbers and use a ramp from
[lynch_sequential](https://gkaramanis.github.io/Lynch/reference/lynch_palettes.md)
or
[lynch_diverging](https://gkaramanis.github.io/Lynch/reference/lynch_palettes.md).

## Usage

``` r
scale_colour_lynch_d(palette = "lost_highway", direction = 1, ...)

scale_fill_lynch_d(palette = "lost_highway", direction = 1, ...)

scale_colour_lynch_c(
  palette = "dune",
  type = c("sequential", "diverging"),
  direction = 1,
  midpoint = NULL,
  guide = "colourbar",
  ...
)

scale_fill_lynch_c(
  palette = "dune",
  type = c("sequential", "diverging"),
  direction = 1,
  midpoint = NULL,
  guide = "colourbar",
  ...
)

scale_color_lynch_d(palette = "lost_highway", direction = 1, ...)

scale_color_lynch_c(
  palette = "dune",
  type = c("sequential", "diverging"),
  direction = 1,
  midpoint = NULL,
  guide = "colourbar",
  ...
)
```

## Arguments

- palette:

  Name of the palette. For the discrete scales, any name in
  `names(lynch_palettes)`; the `_safe` palettes stay distinct with
  red-green colour blindness. For the continuous scales, a name in
  `names(lynch_sequential)` or `names(lynch_diverging)`, depending on
  `type`.

- direction:

  `1` for the palette's order, `-1` to reverse it.

- ...:

  Other arguments passed to
  [`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html)
  or
  [`ggplot2::continuous_scale()`](https://ggplot2.tidyverse.org/reference/continuous_scale.html),
  such as `name`, `labels` or `na.value`.

- type:

  `"sequential"` for ordered values, `"diverging"` for values either
  side of a midpoint.

- midpoint:

  For continuous scales, the data value to put at the centre of the
  ramp, such as `0` for a diverging scale. `NULL` spans the data range.

- guide:

  For continuous scales, the legend: `"colourbar"` (the default),
  `"legend"` or `"none"`.

## Value

A ggplot2 scale.

## Examples

``` r
library(ggplot2)

ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
  geom_point(size = 3) +
  scale_colour_lynch_d("twin_peaks_safe")


ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
  geom_raster() +
  scale_fill_lynch_c("dune")


df <- data.frame(x = 1:20, y = 1, change = seq(-5, 14))
ggplot(df, aes(x, y, fill = change)) +
  geom_tile() +
  scale_fill_lynch_c("mulholland_drive", type = "diverging", midpoint = 0)
```
