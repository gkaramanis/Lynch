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
#>                  palette n_colours normal deutan protan tritan colourblind_safe
#> 1            blue_velvet         5    8.2    8.5    6.7    8.4            FALSE
#> 2                   dune         5    8.5    8.1    9.2    8.4            FALSE
#> 3           lost_highway         5   15.3   15.1   13.2    9.4             TRUE
#> 4       mulholland_drive         5   12.5   10.1    8.8   14.0            FALSE
#> 5             twin_peaks         5   15.3    1.2   14.2   10.2            FALSE
#> 6          wild_at_heart         5   10.0    4.2    7.8   12.1            FALSE
#> 7       blue_velvet_safe         4   17.9   17.2   18.1   10.7             TRUE
#> 8              dune_safe         4   20.5   17.5   11.2   17.9             TRUE
#> 9  mulholland_drive_safe         4   15.4   12.9    8.8   16.5             TRUE
#> 10       twin_peaks_safe         3   28.4   14.5   23.9   35.1             TRUE
#> 11    wild_at_heart_safe         3   21.8   21.3   18.5   21.1             TRUE
#>                        rating
#> 1  close pair for all viewers
#> 2  close pair for all viewers
#> 3           colour-blind safe
#> 4  close pair for all viewers
#> 5       not colour-blind safe
#> 6  close pair for all viewers
#> 7           colour-blind safe
#> 8           colour-blind safe
#> 9           colour-blind safe
#> 10          colour-blind safe
#> 11          colour-blind safe

# Palettes that pass the colour-blindness test
lynch_accessibility[lynch_accessibility$colourblind_safe, "palette"]
#> [1] "lost_highway"          "blue_velvet_safe"      "dune_safe"            
#> [4] "mulholland_drive_safe" "twin_peaks_safe"       "wild_at_heart_safe"   
```
