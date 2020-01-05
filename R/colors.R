#' Complete list of palettes
#'
#' Use \code{\link{lynch_palette}} to construct palettes of desired length.
#'
#' @export

lynch_palettes <- list(
  blue_velvet = c("#2220BD", "#CF3C3D", "#523235", "#C1C6E7", "#333055"),
  dune = c("#1A1454", "#CAAE51", "#8C7573", "#928FA1", "#840F38"),
  lost_highway = c("#DAE9E9", "#F2D85D", "#C0A4AE","#3C2826", "#4F85B8"),
  mullholland_drive = c("#3D87B5", "#000235", "#907978", "#AC3539", "#545671"),
  twin_peaks = c("#332F2F", "#E74F4F", "#47A54E", "#74BDB7", "#F9E9E9"),
  wild_at_heart = c("#CD5C90", "#E5BF5C", "#73364F", "#9F8A5A", "#515767")
)

#' A David Lynch palette generator
#'
#' Color palettes from David Lynch films.
#'
#' @param n Number of colors desired. All palettes have 5 colors. Most color
#'   schemes are derived from \href{https://www.pinterest.se/pin/151152131226891232/}{this Pin on Pinterest}.
#'   If omitted, uses all colours.
#' @param name Name of desired palette. Choices are:
#'   \code{blue_velvet}, \code{dune},  \code{lost_highway},  \code{mullholland_drive},  \code{twin_peaks},  \code{wild_at_heart}
#'   @importFrom graphics rgb rect par image text
#' @return A vector of colours.
#' @export
#' @keywords colors
#' @examples
#' lynch_palette("twin_peaks")
#' lynch_palette("blue_velvet", 3)

lynch_palette <- function(name, n) {
  pal <- lynch_palettes[[name]]
  if (is.null(pal))
    stop("Palette not found.")

  if (missing(n)) {
    n <- length(pal)
  }

  if (n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }

  out <- pal[1:n]
  structure(out, class = "palette", name = name)
}

#' @export
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, col = "#32373D")
}
