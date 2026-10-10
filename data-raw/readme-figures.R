# Builds the README figures in man/figures/. Run from the package root after
# installing the package.
#
# Needs the ragg package and three Google Fonts installed on the system:
# DM Serif Display, Inter and IBM Plex Mono.

library(ggplot2)
library(Lynch)

out <- "man/figures"
ink <- "#1D1B1F"
muted <- "#77727A"
paper <- "#FBF8F3"

films <- data.frame(
  name = c("blue_velvet", "dune", "lost_highway", "mulholland_drive", "twin_peaks", "wild_at_heart"),
  title = c("Blue Velvet", "Dune", "Lost Highway", "Mulholland Drive", "Twin Peaks", "Wild at Heart"),
  year = c("1986", "1984", "1997", "2001", "1990\u201391", "1990")
)

# Polygon for a rectangle with rounded corners.
round_rect <- function(x0, x1, y0, y1, r = 0.12, id = 1) {
  r <- min(r, (x1 - x0) / 2, (y1 - y0) / 2)
  arc <- function(cx, cy, from) {
    a <- seq(from, from + pi / 2, length.out = 8)
    data.frame(x = cx + r * cos(a), y = cy + r * sin(a))
  }
  pts <- rbind(
    arc(x1 - r, y0 + r, -pi / 2),
    arc(x1 - r, y1 - r, 0),
    arc(x0 + r, y1 - r, pi / 2),
    arc(x0 + r, y0 + r, pi)
  )
  pts$id <- id
  pts
}

# A horizontal ramp: thin slices between two rounded end caps.
ramp <- function(cols, x0, x1, y0, y1, id) {
  r <- 0.12
  n <- 150
  fill <- grDevices::colorRampPalette(cols, space = "Lab")(n)
  xs <- seq(x0 + r, x1 - r, length.out = n + 1)
  caps <- rbind(
    transform(round_rect(x0, x0 + 2 * r, y0, y1, r, id = paste(id, "a")), fill = fill[1]),
    transform(round_rect(x1 - 2 * r, x1, y0, y1, r, id = paste(id, "b")), fill = fill[n])
  )
  slices <- do.call(rbind, lapply(seq_len(n), function(i) {
    data.frame(x = xs[c(i, i + 1, i + 1, i)], y = c(y0, y0, y1, y1), id = paste(id, i), fill = fill[i])
  }))
  rbind(caps, slices)
}

text_on <- function(hex) {
  lum <- colSums(grDevices::col2rgb(hex) / 255 * c(0.299, 0.587, 0.114))
  ifelse(lum > 0.55, ink, "#FFFFFF")
}

card <- function(name, title, year) {
  cols <- unclass(lynch_palette(name))
  n <- length(cols)
  w <- 10 / n
  gap <- 0.1

  swatches <- do.call(rbind, Map(function(i, col) {
    r <- round_rect((i - 1) * w + gap / 2, i * w - gap / 2, 1.3, 3.1, id = i)
    r$fill <- col
    r
  }, seq_len(n), cols))

  hex <- data.frame(x = (seq_len(n) - 1) * w + gap / 2 + 0.18, y = 1.5, label = cols, colour = text_on(cols))

  safe_name <- paste0(name, "_safe")
  has_safe <- safe_name %in% names(lynch_palettes)
  safe <- if (has_safe) lynch_palettes[[safe_name]] else character()
  safe_label <- if (has_safe) "colour-blind safe" else "colour-blind safe as it is"
  dots <- data.frame(x = 10 - (rev(seq_along(safe)) - 1) * 0.32 - 0.12, y = rep(3.65, length(safe)), fill = safe)
  label_x <- if (has_safe) min(dots$x) - 0.28 else 10

  ramps <- ramp(lynch_sequential[[name]], 0.05, 4.95, 0.35, 0.75, "seq")
  ramp_labels <- data.frame(x = 0.05, y = 0.98, label = "sequential")
  if (name %in% names(lynch_diverging)) {
    ramps <- rbind(ramps, ramp(lynch_diverging[[name]], 5.05, 9.95, 0.35, 0.75, "div"))
    ramp_labels <- rbind(ramp_labels, data.frame(x = 5.05, y = 0.98, label = "diverging"))
  }

  ggplot() +
    geom_polygon(aes(x, y, group = id, fill = I(fill)), data = swatches) +
    geom_text(aes(x, y, label = label, colour = I(colour)), data = hex,
              hjust = 0, family = "IBM Plex Mono", size = 3.1) +
    geom_polygon(aes(x, y, group = factor(id, unique(id)), fill = I(fill), colour = I(fill)), data = ramps, linewidth = 0.2) +
    geom_text(aes(x, y, label = label), data = ramp_labels, hjust = 0,
              family = "Inter", size = 3, colour = muted) +
    geom_point(aes(x, y, fill = I(fill)), data = dots, shape = 21, size = 4.2, stroke = 0) +
    annotate("text", x = label_x, y = 3.65, label = safe_label, hjust = 1,
             family = "Inter", size = 3, colour = muted) +
    annotate("text", x = 0, y = 3.7, label = title, hjust = 0, family = "DM Serif Display",
             size = 8.5, colour = ink) +
    annotate("text", x = 0.05 + strwidth_in(title), y = 3.62, label = year, hjust = 0,
             family = "Inter", size = 3.4, colour = muted) +
    coord_fixed(xlim = c(0, 10), ylim = c(0.25, 4.1), expand = FALSE) +
    theme_void() +
    theme(plot.background = element_rect(fill = paper, colour = NA),
          plot.margin = margin(14, 18, 10, 18))
}

# Width of the title in plot units, for placing the year after it. The card's
# 10 units span 7.5 inches of drawing area.
strwidth_in <- function(label) {
  px <- systemfonts::string_width(label, family = "DM Serif Display", size = 8.5 * .pt, res = 72)
  px / 72 * 10 / 7.5 + 0.15
}

for (i in seq_len(nrow(films))) {
  f <- films[i, ]
  ggsave(file.path(out, paste0("card-", f$name, ".png")), card(f$name, f$title, f$year),
         device = ragg::agg_png, width = 8, height = 3.3, dpi = 150)
}

# Banner: the Red Room from Twin Peaks. The curtain's folds are shaded with
# the twin_peaks sequential ramp, and the zigzag floor uses the palette's
# near-black and blush.
red_room <- function() {
  shade <- grDevices::colorRamp(lynch_sequential$twin_peaks[7:3], space = "Lab")
  xs <- seq(0, 24, by = 0.04)
  t <- (sin(xs * 2 * pi / 1.3) + 1) / 2
  t <- t^1.6
  curtain <- data.frame(
    xmin = xs, xmax = xs + 0.045, ymin = 2.2, ymax = 8,
    fill = grDevices::rgb(shade(t), maxColorValue = 255)
  )

  zs <- seq(0, 24, by = 0.6)
  floor <- do.call(rbind, lapply(1:9, function(k) {
    edge <- (k - 1) * 0.3 - 0.4 + 0.3 * abs((zs %% 1.2) / 1.2 - 0.5) * 2
    data.frame(x = c(zs, rev(zs)), y = c(edge, rev(edge) + 0.3), id = k,
               fill = if (k %% 2) "#332F2F" else "#F9E9E9")
  }))

  ggplot() +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = I(fill)), data = curtain) +
    geom_polygon(aes(x, y, group = id, fill = I(fill)), data = floor) +
    annotate("text", x = 12, y = 5.6, label = "Lynch", family = "DM Serif Display",
             size = 26, colour = "#F9E9E9") +
    annotate("text", x = 12, y = 3.7, label = "colour palettes from David Lynch films",
             family = "Inter", size = 5, colour = "#F9E9E9") +
    coord_cartesian(xlim = c(0, 24), ylim = c(0, 8), expand = FALSE) +
    theme_void()
}
ggsave(file.path(out, "banner.png"), red_room(), device = ragg::agg_png,
       width = 8, height = 2.7, dpi = 150)

# What printing a palette looks like, for the README examples.
examples <- list(
  "example-twin_peaks" = lynch_palette("twin_peaks"),
  "example-blue_velvet-3" = lynch_palette("blue_velvet", 3),
  "example-dune-sequential" = lynch_palette("dune", 12, type = "sequential"),
  "example-mulholland_drive-diverging" = lynch_palette("mulholland_drive", type = "diverging")
)
for (f in names(examples)) {
  ragg::agg_png(file.path(out, paste0(f, ".png")), width = 900, height = 190, res = 150, background = paper)
  graphics::par(family = "Inter")
  print(examples[[f]])
  dev.off()
}

# Example plots.
plot_theme <- theme_minimal(base_family = "Inter", base_size = 11) +
  theme(
    plot.background = element_rect(fill = paper, colour = NA),
    panel.grid.minor = element_blank(),
    plot.title = element_text(family = "DM Serif Display", size = 16, colour = ink),
    plot.title.position = "plot",
    legend.position = "top",
    legend.justification = "left"
  )

p1 <- ggplot(mpg, aes(displ, hwy, colour = drv)) +
  geom_point(size = 2.2, alpha = 0.85) +
  scale_colour_lynch_d("twin_peaks_safe", labels = c("4wd", "front", "rear")) +
  labs(title = "Twin Peaks, colour-blind safe", x = "Engine size (litres)", y = "Highway mpg", colour = NULL) +
  plot_theme
ggsave(file.path(out, "plot-discrete.png"), p1, device = ragg::agg_png, width = 4.5, height = 3.6, dpi = 150)

p2 <- ggplot(faithfuld, aes(waiting, eruptions, fill = density)) +
  geom_raster(interpolate = TRUE) +
  scale_fill_lynch_c("mulholland_drive", direction = -1, guide = "none") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Mulholland Drive, sequential", x = "Waiting time (min)", y = "Eruption (min)") +
  plot_theme
ggsave(file.path(out, "plot-continuous.png"), p2, device = ragg::agg_png, width = 4.5, height = 3.6, dpi = 150)
