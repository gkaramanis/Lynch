#' Complete list of palettes
#'
#' `lynch_palettes` holds the discrete palettes, five colours each. Every film
#' palette stays distinct with red-green colour blindness except
#' `blue_velvet`; `blue_velvet_safe` keeps the four of its colours that do.
#' `lynch_sequential` holds seven-step light-to-dark ramps in one hue per film,
#' and `lynch_diverging` nine-step ramps between two of a film's colours
#' through neutral grey. See [lynch_accessibility] for the test results.
#'
#' Use [lynch_palette()] to pick colours from any of them.
#'
#' @format Named lists of character vectors of hex colours.
#' @export
#' @examples
#' names(lynch_palettes)
#' lynch_palettes$twin_peaks
#'
#' names(lynch_sequential)
#' names(lynch_diverging)
#'
#' # Use the colours directly in base graphics
#' barplot(1:5, col = lynch_palettes$wild_at_heart)
lynch_palettes <- list(
  blue_velvet = c("#2220BD", "#CF3C3D", "#523235", "#C1C6E7", "#333055"),
  dune = c("#1A1454", "#CAAE51", "#826054", "#928FA1", "#840F38"),
  lost_highway = c("#DAE9E9", "#F2D85D", "#C0A4AE", "#3C2826", "#4F85B8"),
  mulholland_drive = c("#3D87B5", "#000235", "#997B66", "#AC3539", "#545671"),
  twin_peaks = c("#332F2F", "#E74F4F", "#009667", "#74BDB7", "#F9E9E9"),
  wild_at_heart = c("#CD5C90", "#E5BF5C", "#771637", "#A5905F", "#515767"),
  blue_velvet_safe = c("#2220BD", "#CF3C3D", "#C1C6E7", "#333055")
)

#' @rdname lynch_palettes
#' @export
lynch_sequential <- list(
  blue_velvet = c("#DEEDFF", "#B1C7FF", "#87A1F6", "#5F7BE4", "#3B53D1", "#211FBC", "#130085"),
  dune = c("#FFE4E9", "#EFB9C2", "#D5909C", "#BA6778", "#9E3E55", "#800C35", "#55001C"),
  lost_highway = c("#FFEF83", "#E2CA58", "#BAA64B", "#94843E", "#6F6431", "#4C4525", "#2C2918"),
  mulholland_drive = c("#CEF4FF", "#A0D0F2", "#73AED5", "#458CB9", "#2B6A91", "#174A68", "#042C42"),
  twin_peaks = c("#FFD1CA", "#FFA49D", "#FD7671", "#DD4A4A", "#AA3435", "#7B1E20", "#4E0A0E"),
  wild_at_heart = c("#FFD6F2", "#FFABCF", "#E681AC", "#C6588B", "#994069", "#6E2949", "#45132B")
)

#' @rdname lynch_palettes
#' @export
lynch_diverging <- list(
  blue_velvet = c("#211FBD", "#6C51CA", "#9C83D7", "#C5B7E2", "#ECECEC", "#EDC3BD", "#E89990", "#DD6D65", "#CE3B3D"),
  mulholland_drive = c("#3D86B4", "#719FC3", "#9CB8D1", "#C5D2DF", "#ECECEC", "#E3BFBC", "#D4928D", "#C26561", "#AC3439"),
  twin_peaks = c("#73BDB7", "#94C9C4", "#B2D5D1", "#CFE1DF", "#ECECEC", "#F3C7C3", "#F3A29A", "#EF7B73", "#E64E4E"),
  wild_at_heart = c("#771637", "#974F5F", "#B6828B", "#D2B6BA", "#ECECEC", "#DBD4C7", "#C9BCA3", "#B7A680", "#A5905F")
)

#' A David Lynch palette generator
#'
#' ![](logo.png "Lynch logo")
#' Color palettes from David Lynch films.
#'
#' @param name Name of the palette, such as `"twin_peaks"` or
#'   `"blue_velvet_safe"`. See `names(lynch_palettes)`, `names(lynch_sequential)`
#'   and `names(lynch_diverging)`.
#' @param n Number of colours. If omitted, uses all colours. Discrete palettes
#'   offer at most their own colours; sequential and diverging ramps are
#'   interpolated to any `n`. Most discrete colour schemes are derived from
#'   \href{https://www.pinterest.se/pin/151152131226891232/}{this Pin on Pinterest}.
#' @param type `"discrete"` for categories, `"sequential"` for ordered values,
#'   `"diverging"` for values either side of a midpoint.
#' @return A character vector of hex colours with class `palette`. Printing
#'   it draws the colours as swatches; use [unclass()] for a plain vector.
#' @seealso [scale_colour_lynch_d()] and [scale_colour_lynch_c()] for ggplot2.
#' @export
#' @keywords colors
#' @examples
#' lynch_palette("twin_peaks")
#' lynch_palette("blue_velvet", 3)
#' lynch_palette("blue_velvet_safe")
#' lynch_palette("dune", 12, type = "sequential")
#' lynch_palette("mulholland_drive", type = "diverging")
#'
#' # Plain colours for base graphics
#' cols <- unclass(lynch_palette("lost_highway"))
#' pie(rep(1, 5), col = cols)

lynch_palette <- function(name, n, type = c("discrete", "sequential", "diverging")) {
  type <- match.arg(type)

  # Misspelled before 0.2.0; still accepted.
  if (identical(name, "mullholland_drive")) {
    warning('"mullholland_drive" is deprecated; use "mulholland_drive".', call. = FALSE)
    name <- "mulholland_drive"
  }

  # These films became colour-blind safe as they are; the old names still work.
  retired <- c("dune_safe", "mulholland_drive_safe", "twin_peaks_safe", "wild_at_heart_safe")
  if (name %in% retired) {
    film <- sub("_safe$", "", name)
    warning('"', name, '" is deprecated; "', film, '" is colour-blind safe as it is.', call. = FALSE)
    name <- film
  }

  pals <- switch(type,
    discrete = lynch_palettes,
    sequential = lynch_sequential,
    diverging = lynch_diverging
  )
  pal <- pals[[name]]
  if (is.null(pal))
    stop("Palette not found.")

  if (missing(n)) {
    n <- length(pal)
  }

  if (type == "discrete") {
    if (n > length(pal)) {
      stop("Number of requested colors greater than what palette can offer")
    }
    out <- pal[seq_len(n)]
  } else {
    out <- grDevices::colorRampPalette(pal, space = "Lab")(n)
  }

  structure(out, class = "palette", name = name)
}

#' @export
#' @importFrom graphics par plot.new plot.window rect strwidth text
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.2, 0.2, 0.2, 0.2))
  on.exit(par(old))

  plot.new()
  plot.window(xlim = c(0, n), ylim = c(-0.4, 1.35), xaxs = "i", yaxs = "i")

  # Gaps while swatches are wide; hex codes only while they fit underneath.
  gap <- if (n <= 24) 0.06 else 0
  hex <- toupper(x)
  labelled <- max(strwidth(hex, cex = 0.75, family = "mono")) < 1 - 2 * gap

  rect(seq_len(n) - 1 + gap / 2, 0, seq_len(n) - gap / 2, 1, col = x, border = NA)
  if (!is.null(attr(x, "name"))) {
    text(gap / 2, 1.18, labels = attr(x, "name"), adj = c(0, 0.5), font = 2, col = "#1D1B1F")
  }
  if (labelled) {
    text(seq_len(n) - 0.5, -0.2, labels = hex, cex = 0.75, col = "#77727A", family = "mono")
  }
  invisible(x)
}
