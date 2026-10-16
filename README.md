# Lynch

<!-- badges: start -->
[![R-CMD-check](https://github.com/gkaramanis/Lynch/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gkaramanis/Lynch/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

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
  scale_colour_lynch_d("twin_peaks")

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

Most palettes were sampled from stills collected in [this Pinterest pin](https://www.pinterest.se/pin/151152131226891232/). Each one sits beside the film's poster from Wikipedia, so you can hold the two up against each other.

### [Blue Velvet](https://en.wikipedia.org/wiki/Blue_Velvet_(film)) (1986)

`"blue_velvet"` · Frames with a dot are colour-blind safe: `"blue_velvet_safe"`.

<table>
<tr>
<td width="18%"><a href="https://en.wikipedia.org/wiki/Blue_Velvet_(film)"><img src="https://upload.wikimedia.org/wikipedia/en/f/fd/Blue_Velvet_%281986%29.png" alt="Blue Velvet poster" width="100%" /></a></td>
<td><img src="man/figures/card-blue_velvet.png" alt="Blue Velvet palette as a strip of film: five colour frames, the colour-blind-safe ones marked with a dot, sequential and diverging ramps." width="100%" /></td>
</tr>
</table>

<br>

### [Dune](https://en.wikipedia.org/wiki/Dune_(1984_film)) (1984)

`"dune"` · All five colours are colour-blind safe.

<table>
<tr>
<td width="18%"><a href="https://en.wikipedia.org/wiki/Dune_(1984_film)"><img src="https://upload.wikimedia.org/wikipedia/en/5/51/Dune_1984_Poster.jpg" alt="Dune (1984) poster" width="100%" /></a></td>
<td><img src="man/figures/card-dune.png" alt="Dune palette as a strip of film: five colour frames, the colour-blind-safe ones marked with a dot and a sequential ramp." width="100%" /></td>
</tr>
</table>

<br>

### [Lost Highway](https://en.wikipedia.org/wiki/Lost_Highway_(film)) (1997)

`"lost_highway"` · All five colours are colour-blind safe.

<table>
<tr>
<td width="18%"><a href="https://en.wikipedia.org/wiki/Lost_Highway_(film)"><img src="https://upload.wikimedia.org/wikipedia/en/4/47/Lost_Higway_%281997%29.png" alt="Lost Highway poster" width="100%" /></a></td>
<td><img src="man/figures/card-lost_highway.png" alt="Lost Highway palette as a strip of film: five colour frames, all colour-blind safe, and a sequential ramp." width="100%" /></td>
</tr>
</table>

<br>

### [Mulholland Drive](https://en.wikipedia.org/wiki/Mulholland_Drive_(film)) (2001)

`"mulholland_drive"` · All five colours are colour-blind safe.

<table>
<tr>
<td width="18%"><a href="https://en.wikipedia.org/wiki/Mulholland_Drive_(film)"><img src="https://upload.wikimedia.org/wikipedia/en/0/0f/Mulholland.png" alt="Mulholland Drive poster" width="100%" /></a></td>
<td><img src="man/figures/card-mulholland_drive.png" alt="Mulholland Drive palette as a strip of film: five colour frames, the colour-blind-safe ones marked with a dot, sequential and diverging ramps." width="100%" /></td>
</tr>
</table>

<br>

### [Twin Peaks](https://en.wikipedia.org/wiki/Twin_Peaks) (1990–91)

`"twin_peaks"` · All five colours are colour-blind safe. The poster is for *Fire Walk with Me* (1992), the film.

<table>
<tr>
<td width="18%"><a href="https://en.wikipedia.org/wiki/Twin_Peaks"><img src="https://upload.wikimedia.org/wikipedia/en/3/3d/Twin_Peaks_-_Fire_Walk_with_Me.png" alt="Twin Peaks: Fire Walk with Me poster" width="100%" /></a></td>
<td><img src="man/figures/card-twin_peaks.png" alt="Twin Peaks palette as a strip of film: five colour frames, the colour-blind-safe ones marked with a dot, sequential and diverging ramps." width="100%" /></td>
</tr>
</table>

<br>

### [Wild at Heart](https://en.wikipedia.org/wiki/Wild_at_Heart_(film)) (1990)

`"wild_at_heart"` · All five colours are colour-blind safe.

<table>
<tr>
<td width="18%"><a href="https://en.wikipedia.org/wiki/Wild_at_Heart_(film)"><img src="https://upload.wikimedia.org/wikipedia/en/6/60/Wild_at_Heart_film_poster.jpg" alt="Wild at Heart poster" width="100%" /></a></td>
<td><img src="man/figures/card-wild_at_heart.png" alt="Wild at Heart palette as a strip of film: five colour frames, the colour-blind-safe ones marked with a dot, sequential and diverging ramps." width="100%" /></td>
</tr>
</table>

<br>

## Colour blindness

Every palette keeps its five colours apart for people with red-green colour blindness, except Blue Velvet. Its `blue_velvet_safe` version keeps the four colours that do, marked with a dot on its film strip. `lynch_accessibility` has the numbers.

``` r
lynch_palette("blue_velvet_safe")
lynch_accessibility
```

## More

The [Get started vignette](vignettes/Lynch.Rmd) walks through every palette type and scale. The figures on this page are built by `data-raw/readme-figures.R`.

*"Black as midnight on a moonless night."*
