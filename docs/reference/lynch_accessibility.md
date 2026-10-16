# Colour-blindness test results for the discrete palettes

The smallest colour distance between any two colours of each palette in
[lynch_palettes](https://gkaramanis.github.io/Lynch/reference/lynch_palettes.md),
for normal vision and under simulated colour blindness. Distances are in
OKLab (x 100); colour blindness is simulated with colorspace (Machado,
Oliveira and Fernandes 2009, full severity).

## Usage

``` r
lynch_accessibility
```

## Format

A data frame with one row per palette:

- palette:

  Palette name.

- n_colours:

  Number of colours.

- normal, deutan, protan, tritan:

  Smallest distance between two colours with normal vision,
  deuteranopia, protanopia and tritanopia.

- colourblind_safe:

  `TRUE` when every pair is at least 15 apart with normal vision and at
  least 8 apart with deuteranopia and protanopia.

- rating:

  The result in words. "Safe with direct labels" means 6 to 8 apart with
  colour blindness, usable only if the colours are labelled.

## Source

Computed by `data-raw/lynch_accessibility.R` in the package's source
repository.

## Examples

``` r
lynch_accessibility
#>            palette n_colours normal deutan protan tritan colourblind_safe
#> 1      blue_velvet         5    8.2    8.5    6.7    8.4            FALSE
#> 2             dune         5   15.1   11.2   11.2   12.5             TRUE
#> 3     lost_highway         5   15.3   15.1   13.2    9.4             TRUE
#> 4 mulholland_drive         5   15.0    9.6    8.8   15.9             TRUE
#> 5       twin_peaks         5   17.0    8.2    8.8   14.1             TRUE
#> 6    wild_at_heart         5   15.8    8.1   10.8   12.4             TRUE
#> 7 blue_velvet_safe         4   17.9   17.2   18.1   10.7             TRUE
#>                       rating
#> 1 close pair for all viewers
#> 2          colour-blind safe
#> 3          colour-blind safe
#> 4          colour-blind safe
#> 5          colour-blind safe
#> 6          colour-blind safe
#> 7          colour-blind safe

# Palettes that pass the colour-blindness test
lynch_accessibility[lynch_accessibility$colourblind_safe, "palette"]
#> [1] "dune"             "lost_highway"     "mulholland_drive" "twin_peaks"      
#> [5] "wild_at_heart"    "blue_velvet_safe"
```
