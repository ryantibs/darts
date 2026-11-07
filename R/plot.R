#' Draw a dartboard
#'
#' Draws the outline of a dartboard, either as a new plot or on top of an
#' existing one, such as a heatmap from [drawHeatmap()]. Coordinates are in
#' millimeters, relative to the center of the board.
#'
#' @param new If `TRUE`, start a new plot; otherwise draw on the current one.
#' @param lines Draw the wires?
#' @param numbers Draw the numbers?
#' @param outside If `TRUE`, the numbers are drawn outside the board;
#'   otherwise inside it.
#' @param col Color of the wires and numbers.
#' @param ... Further arguments passed to [graphics::text()] when drawing
#'   the numbers, e.g. `cex`.
#' @return `NULL`, invisibly.
#' @export
#' @examples
#' drawBoard(new = TRUE)
drawBoard <- function(new = FALSE, lines = TRUE, numbers = TRUE,
                      outside = TRUE, col = "black", ...) {
  R <- board_geom$R
  Rmax <- board_radius()

  if (new) {
    graphics::plot.new()
    graphics::plot.window(xlim = c(-Rmax, Rmax) * 1.1,
                          ylim = c(-Rmax, Rmax) * 1.1, asp = 1)
  }

  if (lines) {
    # Rings
    t <- seq(0, 2 * pi, length.out = 1000)
    for (r in R) graphics::lines(r * cos(t), r * sin(t), col = col)

    # Radial wires between slices, from the single bullseye outward
    t <- pi / 2 + pi / 20 - (0:19) * 2 * pi / 20
    graphics::segments(R[2] * cos(t), R[2] * sin(t),
                       Rmax * cos(t), Rmax * sin(t), col = col)
  }

  if (numbers) {
    r <- if (outside) Rmax + 10 else 0.8 * Rmax
    t <- pi / 2 - (0:19) * 2 * pi / 20
    graphics::text(r * cos(t), r * sin(t), board_geom$S, col = col, ...)
  }

  invisible(NULL)
}

#' Draw a heatmap of expected scores
#'
#' Plots the expected score for each aiming location, with lighter colors
#' for higher expected scores.
#'
#' @param e Matrix of expected scores, from [simpleExpScores()] or
#'   [generalExpScores()].
#' @param col Color palette, from low to high expected scores.
#' @param board If `TRUE`, overlay the dartboard with [drawBoard()].
#' @param aim If `TRUE`, mark the optimal aiming location with
#'   [drawAimSpot()].
#' @return `NULL`, invisibly.
#' @export
#' @examples
#' e <- simpleExpScores(simpleEM(throws)$s.final)
#' op <- par(mfrow = c(1, 2), mar = c(0, 0, 0, 0))
#' drawHeatmap(e)
#' drawHeatmap(e, board = TRUE, aim = TRUE)
#' par(op)
drawHeatmap <- function(e, col = grDevices::heat.colors(30), board = FALSE,
                        aim = FALSE) {
  check_heatmap(e)
  R <- board_radius()
  graphics::plot.new()
  graphics::plot.window(xlim = c(-R, R) * 1.1, ylim = c(-R, R) * 1.1, asp = 1)
  graphics::image(-R:R, -R:R, e, col = col, add = TRUE)
  graphics::rect(-R, -R, R, R)
  if (board) drawBoard(numbers = FALSE)
  if (aim) drawAimSpot(e)
  invisible(NULL)
}

#' Mark the optimal aiming location
#'
#' Adds a point at the aiming location with the highest expected score to
#' the current plot, as found by [aimSpot()].
#'
#' @inheritParams drawHeatmap
#' @param col,pch Color and plotting symbol of the point.
#' @param ... Further arguments passed to [graphics::points()].
#' @return The result of [aimSpot()], invisibly.
#' @export
#' @examples
#' e <- simpleExpScores(simpleEM(throws)$s.final)
#' drawBoard(new = TRUE)
#' drawAimSpot(e)
drawAimSpot <- function(e, col = "blue", pch = 19, ...) {
  a <- aimSpot(e)
  graphics::points(a["x"], a["y"], col = col, pch = pch, ...)
  invisible(a)
}
