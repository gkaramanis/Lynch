# Lynch 0.4.0

* New ggplot2 scales: `scale_colour_lynch_d()` and `scale_fill_lynch_d()` for categories, `scale_colour_lynch_c()` and `scale_fill_lynch_c()` for numbers, with sequential or diverging ramps and an optional `midpoint`. `scale_color_*` aliases included.

# Lynch 0.3.0

* Colour-blind-safe variants of five palettes: `blue_velvet_safe`, `dune_safe`, `mulholland_drive_safe`, `twin_peaks_safe` and `wild_at_heart_safe`. Each keeps the three or four colours of the film's palette that stay distinct with red-green colour blindness. `lost_highway` already passes as it is.
* New sequential ramps (`lynch_sequential`, one per film) and diverging ramps (`lynch_diverging`, for four films). `lynch_palette()` gains `type = "sequential"` and `type = "diverging"` and interpolates ramps to any `n`.
* New `lynch_accessibility`: colour-blindness test results for every discrete palette.

# Lynch 0.2.0

* The Mulholland Drive palette is now `mulholland_drive`. The old spelling, `mullholland_drive`, still works with a deprecation warning.
* Added tests.
