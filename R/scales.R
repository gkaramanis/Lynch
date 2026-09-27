# Palette function for discrete scales. Levels beyond the palette's colours get
# NA, which ggplot2 draws in its na.value colour.
lynch_pal_d <- function(palette, direction) {
  cols <- lynch_palettes[[palette]]
  if (is.null(cols)) {
    stop("Palette not found.", call. = FALSE)
  }
  if (direction < 0) cols <- rev(cols)
  function(n) {
    if (n > length(cols)) {
      warning(
        "Palette \"", palette, "\" has ", length(cols), " colours, but ", n,
        " are needed. The extra levels will be shown as NA.",
        call. = FALSE
      )
    }
    cols[seq_len(n)]
  }
}

# Continuous scale over a sequential or diverging ramp. A midpoint centres the
# ramp on that value; otherwise the ramp spans the data range.
lynch_scale_c <- function(aesthetics, palette, type, direction, midpoint, ...) {
  type <- match.arg(type, c("sequential", "diverging"))
  cols <- unclass(lynch_palette(palette, type = type))
  if (direction < 0) cols <- rev(cols)

  rescaler <- if (is.null(midpoint)) {
    scales::rescale
  } else {
    function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
      scales::rescale_mid(x, to, from, midpoint)
    }
  }

  ggplot2::continuous_scale(
    aesthetics,
    palette = scales::gradient_n_pal(cols),
    rescaler = rescaler,
    guide = "colourbar",
    ...
  )
}

#' Lynch scales for ggplot2
#'
#' Colour and fill scales using the Lynch palettes. The `_d` scales are for
#' categories and use a discrete palette from [lynch_palettes]; the `_c` scales
#' are for numbers and use a ramp from [lynch_sequential] or [lynch_diverging].
#'
#' @param palette Name of the palette. For the discrete scales, any name in
#'   `names(lynch_palettes)`; the `_safe` palettes stay distinct with red-green
#'   colour blindness. For the continuous scales, a name in
#'   `names(lynch_sequential)` or `names(lynch_diverging)`, depending on `type`.
#' @param type `"sequential"` for ordered values, `"diverging"` for values
#'   either side of a midpoint.
#' @param direction `1` for the palette's order, `-1` to reverse it.
#' @param midpoint For continuous scales, the data value to put at the centre
#'   of the ramp, such as `0` for a diverging scale. `NULL` spans the data
#'   range.
#' @param ... Other arguments passed to [ggplot2::discrete_scale()] or
#'   [ggplot2::continuous_scale()], such as `name`, `labels` or `na.value`.
#' @return A ggplot2 scale.
#' @export
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point(size = 3) +
#'   scale_colour_lynch_d("twin_peaks_safe")
#'
#' ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
#'   geom_raster() +
#'   scale_fill_lynch_c("dune")
#'
#' df <- data.frame(x = 1:20, y = 1, change = seq(-5, 14))
#' ggplot(df, aes(x, y, fill = change)) +
#'   geom_tile() +
#'   scale_fill_lynch_c("mulholland_drive", type = "diverging", midpoint = 0)
scale_colour_lynch_d <- function(palette = "lost_highway", direction = 1, ...) {
  ggplot2::discrete_scale("colour", palette = lynch_pal_d(palette, direction), ...)
}

#' @rdname scale_colour_lynch_d
#' @export
scale_fill_lynch_d <- function(palette = "lost_highway", direction = 1, ...) {
  ggplot2::discrete_scale("fill", palette = lynch_pal_d(palette, direction), ...)
}

#' @rdname scale_colour_lynch_d
#' @export
scale_colour_lynch_c <- function(palette = "dune", type = c("sequential", "diverging"),
                                 direction = 1, midpoint = NULL, ...) {
  lynch_scale_c("colour", palette, type, direction, midpoint, ...)
}

#' @rdname scale_colour_lynch_d
#' @export
scale_fill_lynch_c <- function(palette = "dune", type = c("sequential", "diverging"),
                               direction = 1, midpoint = NULL, ...) {
  lynch_scale_c("fill", palette, type, direction, midpoint, ...)
}

#' @rdname scale_colour_lynch_d
#' @export
scale_color_lynch_d <- scale_colour_lynch_d

#' @rdname scale_colour_lynch_d
#' @export
scale_color_lynch_c <- scale_colour_lynch_c
