# Colour-blindness checks for categorical palettes.
#
# Distances are Euclidean in OKLab x 100. Colour blindness is simulated with
# colorspace (Machado, Oliveira and Fernandes 2009, severity 1). Thresholds:
# every pair at least 15 apart with normal vision, and at least 8 apart under
# protanopia and deuteranopia; 6 to 8 is usable only with direct labels.

min_distances <- function(cols) {
  de <- function(x) {
    lab <- farver::convert_colour(t(grDevices::col2rgb(x)), "rgb", "oklab") * 100
    d <- as.matrix(stats::dist(lab))
    min(d[upper.tri(d)])
  }
  c(
    normal = de(cols),
    deutan = de(colorspace::deutan(cols)),
    protan = de(colorspace::protan(cols)),
    tritan = de(colorspace::tritan(cols))
  )
}

accessibility_table <- function(palettes) {
  rows <- lapply(names(palettes), function(name) {
    m <- min_distances(palettes[[name]])
    cvd <- min(m[["deutan"]], m[["protan"]])
    data.frame(
      palette = name,
      n_colours = length(palettes[[name]]),
      normal = round(m[["normal"]], 1),
      deutan = round(m[["deutan"]], 1),
      protan = round(m[["protan"]], 1),
      tritan = round(m[["tritan"]], 1),
      colourblind_safe = m[["normal"]] >= 15 && cvd >= 8,
      rating = if (m[["normal"]] < 15) {
        "close pair for all viewers"
      } else if (cvd >= 8) {
        "colour-blind safe"
      } else if (cvd >= 6) {
        "safe with direct labels"
      } else {
        "not colour-blind safe"
      }
    )
  })
  do.call(rbind, rows)
}
