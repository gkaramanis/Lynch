#' Colour-blindness test results for the discrete palettes
#'
#' The smallest colour distance between any two colours of each palette in
#' [lynch_palettes], for normal vision and under simulated colour blindness.
#' Distances are in OKLab (x 100); colour blindness is simulated with
#' \pkg{colorspace} (Machado, Oliveira and Fernandes 2009, full severity).
#'
#' @format A data frame with one row per palette:
#' \describe{
#'   \item{palette}{Palette name.}
#'   \item{n_colours}{Number of colours.}
#'   \item{normal, deutan, protan, tritan}{Smallest distance between two
#'     colours with normal vision, deuteranopia, protanopia and tritanopia.}
#'   \item{colourblind_safe}{`TRUE` when every pair is at least 15 apart with
#'     normal vision and at least 8 apart with deuteranopia and protanopia.}
#'   \item{rating}{The result in words. "Safe with direct labels" means 6 to 8
#'     apart with colour blindness, usable only if the colours are labelled.}
#' }
#' @examples
#' lynch_accessibility[lynch_accessibility$colourblind_safe, "palette"]
"lynch_accessibility"
