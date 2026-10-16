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

# A flat horizontal ramp made of thin slices.
ramp <- function(cols, x0, x1, y0, y1, id) {
  n <- 200
  xs <- seq(x0, x1, length.out = n + 1)
  data.frame(
    xmin = xs[-(n + 1)], xmax = xs[-1], ymin = y0, ymax = y1,
    fill = grDevices::colorRampPalette(cols, space = "Lab")(n)
  )
}

# A china-marker box around a frame: four bowed strokes of uneven width that
# run past the corners, as when choosing frames on a contact sheet.
grease_box <- function(x0, x1, y0, y1, id, seed = id) {
  set.seed(seed)
  stroke <- function(xa, ya, xb, yb, k) {
    t <- seq(0, 1, length.out = 40)
    bow <- stats::runif(1, -0.015, 0.015) * sin(pi * t)
    shift <- stats::runif(2, -0.006, 0.006)
    dx <- xb - xa
    dy <- yb - ya
    len <- sqrt(dx^2 + dy^2)
    data.frame(
      x = xa + dx * t - dy / len * bow + shift[1],
      y = ya + dy * t + dx / len * bow + shift[2],
      width = 0.8 + 0.6 * sin(pi * t)^0.6 * stats::runif(1, 0.6, 1) +
        0.12 * sin(t * stats::runif(1, 8, 14)),
      id = paste(id, k)
    )
  }
  over <- stats::runif(8, 0.02, 0.08)
  rbind(
    stroke(x0 - over[1], y1, x1 + over[2], y1, 1),
    stroke(x1, y1 + over[3], x1, y0 - over[4], 2),
    stroke(x1 + over[5], y0, x0 - over[6], y0, 3),
    stroke(x0, y0 - over[7], x0, y1 + over[8], 4)
  )
}

# Each palette as a strip of 35 mm film on a contact sheet: one colour per
# frame, the hex codes and title in the edge print, and the colour-blind-safe
# frames boxed in red china marker.
card <- function(name, title, year) {
  cols <- unclass(lynch_palette(name))
  n <- length(cols)
  pitch <- 10 / n
  base <- "#161313"
  edge_ink <- "#E39B3A"
  pencil <- "#D8322B"

  strip <- c(0.95, 3.15)
  mid <- mean(strip)
  frame_h <- 0.63 * pitch
  frames <- do.call(rbind, Map(function(i, col) {
    r <- round_rect((i - 1) * pitch + 0.05, i * pitch - 0.05,
                    mid - frame_h / 2, mid + frame_h / 2, r = 0.035, id = i)
    r$fill <- col
    r
  }, seq_len(n), cols))

  perf_x <- seq(-pitch / 16, 10 + pitch / 8, by = pitch / 8)
  perfs <- do.call(rbind, lapply(seq_along(perf_x), function(k) {
    x <- perf_x[k]
    rbind(
      round_rect(x - 0.055, x + 0.055, strip[1] + 0.07, strip[1] + 0.22, r = 0.025, id = paste0("b", k)),
      round_rect(x - 0.055, x + 0.055, strip[2] - 0.22, strip[2] - 0.07, r = 0.025, id = paste0("t", k))
    )
  }))

  top_y <- strip[2] - 0.33
  bottom_y <- strip[1] + 0.33
  frame_x <- (seq_len(n) - 1) * pitch + 0.1
  title_print <- data.frame(
    x = frame_x,
    label = paste(toupper(title), year, sep = "  ")
  )
  numbers <- data.frame(
    x = c(frame_x, frame_x + pitch / 2),
    label = c(seq_len(n), paste0("▸", seq_len(n), "A"))
  )
  hex <- data.frame(x = frame_x + 0.42, label = sub("#", "", cols))

  safe_name <- paste0(name, "_safe")
  has_safe <- safe_name %in% names(lynch_palettes)
  safe <- if (has_safe) match(lynch_palettes[[safe_name]], cols) else integer()
  boxes <- do.call(rbind, lapply(safe, function(i) {
    grease_box((i - 1) * pitch + 0.025, i * pitch - 0.025,
               mid - frame_h / 2 - 0.035, mid + frame_h / 2 + 0.035, id = i,
               seed = sum(utf8ToInt(name)) + i)
  }))

  ramps <- ramp(lynch_sequential[[name]], 0.1, 4.9, 0.32, 0.5)
  ramp_labels <- data.frame(x = 0.1, label = "sequential")
  if (name %in% names(lynch_diverging)) {
    ramps <- rbind(ramps, ramp(lynch_diverging[[name]], 5.1, 9.9, 0.32, 0.5))
    ramp_labels <- rbind(ramp_labels, data.frame(x = 5.1, label = "diverging"))
  }

  ggplot() +
    annotate("rect", xmin = -1, xmax = 11, ymin = strip[1], ymax = strip[2], fill = base) +
    geom_polygon(aes(x, y, group = id), data = perfs, fill = paper) +
    geom_polygon(aes(x, y, group = id, fill = I(fill)), data = frames) +
    geom_text(aes(x, top_y, label = label), data = title_print, hjust = 0,
              family = "IBM Plex Mono", fontface = "bold", size = 2.6, colour = edge_ink) +
    geom_text(aes(x, bottom_y, label = label), data = numbers, hjust = 0,
              family = "IBM Plex Mono", size = 2.6, colour = edge_ink) +
    geom_text(aes(x, bottom_y, label = label), data = hex, hjust = 0,
              family = "IBM Plex Mono", size = 2.6, colour = edge_ink) +
    {if (has_safe) geom_path(aes(x, y, group = id, linewidth = I(width)), data = boxes,
                             colour = pencil, lineend = "round", alpha = 0.92)} +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = I(fill)),
              data = ramps, colour = NA) +
    geom_text(aes(x, 0.63, label = label), data = ramp_labels, hjust = 0,
              family = "IBM Plex Mono", size = 2.8, colour = muted) +
    coord_fixed(xlim = c(0, 10), ylim = c(0.2, 3.3), expand = FALSE) +
    theme_void() +
    theme(plot.background = element_rect(fill = paper, colour = NA),
          plot.margin = margin(0, 0, 0, 0))
}

for (i in seq_len(nrow(films))) {
  f <- films[i, ]
  ggsave(file.path(out, paste0("card-", f$name, ".png")), card(f$name, f$title, f$year),
         device = ragg::agg_png, width = 8, height = 2.48, dpi = 150)
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
