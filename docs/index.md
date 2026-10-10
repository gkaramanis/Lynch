# Lynch

![The Red Room from Twin Peaks: a red curtain over a black-and-white
zigzag floor, with the word Lynch.](reference/figures/banner.png)

Colour palettes from the films of David Lynch, for when a bar chart
needs a little dread.

Every film gets five colours. Most also get a colour-blind-safe cut, a
sequential ramp for numbers, and sometimes a diverging one. It’s a fun
package first, but the scales plug straight into ggplot2, and every
palette has been through a colour-blindness simulator.

## Installation

\
`# install.packages("pak")`\
`pak``::`[`pak`](https://pak.r-lib.org/reference/pak.html)`(``"gkaramanis/Lynch"``)`

## Quick start

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`Lynch`](https://github.com/gkaramanis/Lynch)`)`\
\
[`lynch_palette`](https://gkaramanis.github.io/Lynch/reference/lynch_palette.md)`(``"twin_peaks"``)`

![The Twin Peaks palette: five swatches with their hex
codes.](reference/figures/example-twin_peaks.png)

\
[`lynch_palette`](https://gkaramanis.github.io/Lynch/reference/lynch_palette.md)`(``"blue_velvet"``, ``3``)`

![The first three colours of Blue
Velvet.](reference/figures/example-blue_velvet-3.png)

Ramps stretch to as many colours as you like:

\
[`lynch_palette`](https://gkaramanis.github.io/Lynch/reference/lynch_palette.md)`(``"dune"``, ``12``, type ``=`` ``"sequential"``)`\
[`lynch_palette`](https://gkaramanis.github.io/Lynch/reference/lynch_palette.md)`(``"mulholland_drive"``, type ``=`` ``"diverging"``)`

![Twelve steps of the Dune sequential ramp, pale pink to deep
maroon.](reference/figures/example-dune-sequential.png)![The Mulholland
Drive diverging ramp, blue through grey to
red.](reference/figures/example-mulholland_drive-diverging.png)

## With ggplot2

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`ggplot2`](https://ggplot2.tidyverse.org)`)`\
\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``mpg``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``displ``, ``hwy``, colour ``=`` ``drv``)``)`` ``+`\
`  `[`geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html)`(``)`` ``+`\
`  `[`scale_colour_lynch_d`](https://gkaramanis.github.io/Lynch/reference/scale_colour_lynch_d.md)`(``"twin_peaks_safe"``)`\
\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``faithfuld``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``waiting``, ``eruptions``, fill ``=`` ``density``)``)`` ``+`\
`  `[`geom_raster`](https://ggplot2.tidyverse.org/reference/geom_tile.html)`(``)`` ``+`\
`  `[`scale_fill_lynch_c`](https://gkaramanis.github.io/Lynch/reference/scale_colour_lynch_d.md)`(``"mulholland_drive"``, direction ``=`` ``-``1``)`

![Scatter plot of engine size against highway mileage, coloured by drive
type in the Twin Peaks safe
palette.](reference/figures/plot-discrete.png)![Density map of Old
Faithful eruptions in the Mulholland Drive ramp, glowing blue on
near-black.](reference/figures/plot-continuous.png)

`_d` scales are for categories and `_c` scales for numbers. For a
diverging scale, add `type = "diverging"` and a `midpoint`.

## The palettes

Most palettes were sampled from stills collected in [this Pinterest
pin](https://www.pinterest.se/pin/151152131226891232/). Each film’s
title below links to its Wikipedia article, where you can hold the
poster up against the colours.

![Blue Velvet palette card: five colours, a colour-blind-safe subset,
sequential and diverging
ramps.](reference/figures/card-blue_velvet.png)[**Blue
Velvet**](https://en.wikipedia.org/wiki/Blue_Velvet_(film)) ·
`"blue_velvet"`

![Dune palette card: five colours, a colour-blind-safe subset and a
sequential
ramp.](reference/figures/card-dune.png)[**Dune**](https://en.wikipedia.org/wiki/Dune_(1984_film))
· `"dune"`

![Lost Highway palette card: five colours, colour-blind safe as they
are, and a sequential
ramp.](reference/figures/card-lost_highway.png)[**Lost
Highway**](https://en.wikipedia.org/wiki/Lost_Highway_(film)) ·
`"lost_highway"`

![Mulholland Drive palette card: five colours, a colour-blind-safe
subset, sequential and diverging
ramps.](reference/figures/card-mulholland_drive.png)[**Mulholland
Drive**](https://en.wikipedia.org/wiki/Mulholland_Drive_(film)) ·
`"mulholland_drive"`

![Twin Peaks palette card: five colours, a colour-blind-safe subset,
sequential and diverging
ramps.](reference/figures/card-twin_peaks.png)[**Twin
Peaks**](https://en.wikipedia.org/wiki/Twin_Peaks) · `"twin_peaks"`

![Wild at Heart palette card: five colours, a colour-blind-safe subset,
sequential and diverging
ramps.](reference/figures/card-wild_at_heart.png)[**Wild at
Heart**](https://en.wikipedia.org/wiki/Wild_at_Heart_(film)) ·
`"wild_at_heart"`

## Colour blindness

Only Lost Highway keeps all five colours apart for people with red-green
colour blindness. The other films have a `_safe` version that keeps the
three or four colours that do, shown as dots on each card.
`lynch_accessibility` has the numbers.

\
[`lynch_palette`](https://gkaramanis.github.io/Lynch/reference/lynch_palette.md)`(``"twin_peaks_safe"``)`\
`lynch_accessibility`

## More

The [Get started
vignette](https://gkaramanis.github.io/Lynch/vignettes/Lynch.Rmd) walks
through every palette type and scale. The figures on this page are built
by `data-raw/readme-figures.R`.

*“Black as midnight on a moonless night.”*
