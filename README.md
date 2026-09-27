
# Lynch

<!-- badges: start -->
<!-- badges: end -->

Color palettes from David Lynch films.

## Installation

Install Lynch from GitHub with:

``` r
# install.packages("pak")
pak::pak("gkaramanis/Lynch")
```

## Example

``` r
library(Lynch)  

lynch_palette("twin_peaks") 
```
![Twin Peaks palette: five colour swatches](man/figures/example-twin_peaks.png)  

``` r
lynch_palette("blue_velvet", 3)
```
![First three colours of the Blue Velvet palette](man/figures/example-blue_velvet-3.png)  

## Available palettes
![Blue Velvet palette: five colour swatches](man/figures/blue_velvet.png)
![Dune palette: five colour swatches](man/figures/dune.png)
![Lost Highway palette: five colour swatches](man/figures/lost_highway.png)
![Mulholland Drive palette: five colour swatches](man/figures/mulholland_drive.png)
![Twin Peaks palette: five colour swatches](man/figures/twin_peaks.png)
![Wild at Heart palette: five colour swatches](man/figures/wild_at_heart.png)

## Colour-blind-safe palettes

`lynch_accessibility` gives each discrete palette's smallest colour difference, with normal vision and simulated colour blindness. Of the film palettes, only `lost_highway` keeps every colour distinct with red-green colour blindness. The `_safe` palettes keep the colours of each film's palette that do:

![The five colour-blind-safe palettes, each with three or four colour swatches.](man/figures/safe.png)

``` r
lynch_palette("twin_peaks_safe")
```

## Sequential and diverging ramps

For ordered values, use a sequential ramp; for values either side of a midpoint, a diverging one. Both interpolate to any number of colours:

``` r
lynch_palette("dune", 12, type = "sequential")
lynch_palette("mulholland_drive", type = "diverging")
```

![Sequential ramps, one per film, each running from a light tint to a dark shade of one hue.](man/figures/sequential.png)

![Diverging ramps for four films, each running from one of the film's colours through light grey to another.](man/figures/diverging.png)

