# Complete list of palettes

`lynch_palettes` holds the discrete palettes, five colours each. The
`*_safe` palettes keep the three or four colours of a film's palette
that stay distinct with red-green colour blindness; `lost_highway`
already does. `lynch_sequential` holds seven-step light-to-dark ramps in
one hue per film, and `lynch_diverging` nine-step ramps between two of a
film's colours through neutral grey. See
[lynch_accessibility](https://gkaramanis.github.io/Lynch/reference/lynch_accessibility.md)
for the test results.

## Usage

``` r
lynch_palettes

lynch_sequential

lynch_diverging
```

## Format

Named lists of character vectors of hex colours.

## Details

Use
[`lynch_palette()`](https://gkaramanis.github.io/Lynch/reference/lynch_palette.md)
to pick colours from any of them.

## Examples

``` r
names(lynch_palettes)
#>  [1] "blue_velvet"           "dune"                  "lost_highway"         
#>  [4] "mulholland_drive"      "twin_peaks"            "wild_at_heart"        
#>  [7] "blue_velvet_safe"      "dune_safe"             "mulholland_drive_safe"
#> [10] "twin_peaks_safe"       "wild_at_heart_safe"   
lynch_palettes$twin_peaks
#> [1] "#332F2F" "#E74F4F" "#47A54E" "#74BDB7" "#F9E9E9"

names(lynch_sequential)
#> [1] "blue_velvet"      "dune"             "lost_highway"     "mulholland_drive"
#> [5] "twin_peaks"       "wild_at_heart"   
names(lynch_diverging)
#> [1] "blue_velvet"      "mulholland_drive" "twin_peaks"       "wild_at_heart"   

# Use the colours directly in base graphics
barplot(1:5, col = lynch_palettes$wild_at_heart)
```
