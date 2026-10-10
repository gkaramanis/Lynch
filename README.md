# Lynch

<!-- badges: start -->
[![R-CMD-check](https://github.com/gkaramanis/Lynch/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gkaramanis/Lynch/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

<img src="man/figures/banner.png" alt="The Red Room from Twin Peaks: a red curtain over a black-and-white zigzag floor, with the word Lynch." width="100%" />

Colour palettes from the films of David Lynch, for when a bar chart needs a little dread.

Every film gets five colours. Most also get a colour-blind-safe cut, a sequential ramp for numbers, and sometimes a diverging one. It's a fun package first, but the scales plug straight into ggplot2, and every palette has been through a colour-blindness simulator.

## Installation

``` r
# install.packages("pak")
pak::pak("gkaramanis/Lynch")
```

## Quick start

``` r
library(Lynch)

lynch_palette("twin_peaks")
```

<img src="man/figures/example-twin_peaks.png" alt="The Twin Peaks palette: five swatches with their hex codes." width="100%" />

``` r
lynch_palette("blue_velvet", 3)
```

<img src="man/figures/example-blue_velvet-3.png" alt="The first three colours of Blue Velvet." width="100%" />

Ramps stretch to as many colours as you like:

``` r
lynch_palette("dune", 12, type = "sequential")
lynch_palette("mulholland_drive", type = "diverging")
```

<img src="man/figures/example-dune-sequential.png" alt="Twelve steps of the Dune sequential ramp, pale pink to deep maroon." width="100%" />
<img src="man/figures/example-mulholland_drive-diverging.png" alt="The Mulholland Drive diverging ramp, blue through grey to red." width="100%" />

## With ggplot2

``` r
library(ggplot2)

ggplot(mpg, aes(displ, hwy, colour = drv)) +
  geom_point() +
  scale_colour_lynch_d("twin_peaks_safe")

ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
  geom_raster() +
  scale_fill_lynch_c("mulholland_drive", direction = -1)
```

<p>
<img src="man/figures/plot-discrete.png" width="49%" alt="Scatter plot of engine size against highway mileage, coloured by drive type in the Twin Peaks safe palette." />
<img src="man/figures/plot-continuous.png" width="49%" alt="Density map of Old Faithful eruptions in the Mulholland Drive ramp, glowing blue on near-black." />
</p>

`_d` scales are for categories and `_c` scales for numbers. For a diverging scale, add `type = "diverging"` and a `midpoint`.

## The palettes

Most palettes were sampled from stills collected in [this Pinterest pin](https://www.pinterest.se/pin/151152131226891232/). Each film's title below links to its Wikipedia article, where you can hold the poster up against the colours.

<img src="man/figures/card-blue_velvet.png" alt="Blue Velvet palette card: five colours, a colour-blind-safe subset, sequential and diverging ramps." width="100%" />
[**Blue Velvet**](https://en.wikipedia.org/wiki/Blue_Velvet_(film)) · `"blue_velvet"`

<img src="man/figures/card-dune.png" alt="Dune palette card: five colours, a colour-blind-safe subset and a sequential ramp." width="100%" />
[**Dune**](https://en.wikipedia.org/wiki/Dune_(1984_film)) · `"dune"`

<img src="man/figures/card-lost_highway.png" alt="Lost Highway palette card: five colours, colour-blind safe as they are, and a sequential ramp." width="100%" />
[**Lost Highway**](https://en.wikipedia.org/wiki/Lost_Highway_(film)) · `"lost_highway"`

<img src="man/figures/card-mulholland_drive.png" alt="Mulholland Drive palette card: five colours, a colour-blind-safe subset, sequential and diverging ramps." width="100%" />
[**Mulholland Drive**](https://en.wikipedia.org/wiki/Mulholland_Drive_(film)) · `"mulholland_drive"`

<img src="man/figures/card-twin_peaks.png" alt="Twin Peaks palette card: five colours, a colour-blind-safe subset, sequential and diverging ramps." width="100%" />
[**Twin Peaks**](https://en.wikipedia.org/wiki/Twin_Peaks) · `"twin_peaks"`

<img src="man/figures/card-wild_at_heart.png" alt="Wild at Heart palette card: five colours, a colour-blind-safe subset, sequential and diverging ramps." width="100%" />
[**Wild at Heart**](https://en.wikipedia.org/wiki/Wild_at_Heart_(film)) · `"wild_at_heart"`

## Colour blindness

Only Lost Highway keeps all five colours apart for people with red-green colour blindness. The other films have a `_safe` version that keeps the three or four colours that do, shown as dots on each card. `lynch_accessibility` has the numbers.

``` r
lynch_palette("twin_peaks_safe")
lynch_accessibility
```

## More

The [Get started vignette](vignettes/Lynch.Rmd) walks through every palette type and scale. The figures on this page are built by `data-raw/readme-figures.R`.

*"Black as midnight on a moonless night."*
