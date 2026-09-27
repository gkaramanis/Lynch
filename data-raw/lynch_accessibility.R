# Builds data/lynch_accessibility.rda. Run from the package root.

source("data-raw/cvd.R")
source("R/colors.R")

lynch_accessibility <- accessibility_table(lynch_palettes)
usethis::use_data(lynch_accessibility, overwrite = TRUE)
