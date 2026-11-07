#' Expected scores across the board
#'
#' Computes the expected score of a dart for every aiming location on the
#' board, on a 1mm grid, assuming throws land at a Gaussian location centered
#' at the aiming spot. The expectation is a convolution of the board's score
#' function with the Gaussian density, computed with the fast Fourier
#' transform.
#'
#' @param s Variance of each coordinate, in squared millimeters, as estimated
#'   by [simpleEM()].
#' @return A 341 x 341 matrix `e`, where `e[i, j]` is the expected score
#'   when aiming at `x = i - 171`, `y = j - 171` (in mm, relative to the
#'   center of the board). The row and column names give these coordinates.
#' @seealso [drawHeatmap()] to plot the result, and [aimSpot()] to find the
#'   best place to aim.
#' @export
#' @examples
#' e <- simpleExpScores(simpleEM(throws)$s.final)
#' aimSpot(e)
simpleExpScores <- function(s) {
  check_positive(s, "s")
  generalExpScores(c(s, s, 0))
}

#' @rdname simpleExpScores
#' @param Sig Covariance matrix, as a vector `c(var_x, var_y, cov_xy)` in
#'   squared millimeters, as estimated by [generalEM()].
#' @export
#' @examples
#'
#' e <- generalExpScores(c(400, 900, 100))
#' aimSpot(e)
generalExpScores <- function(Sig) {
  check_cov(Sig, "Sig")
  R <- board_radius()
  A <- score_matrix()

  # Gaussian density on the same grid, centered at the middle
  d <- -(2 * R):(2 * R)
  x <- matrix(d, length(d), length(d))
  y <- t(x)
  det <- Sig[1] * Sig[2] - Sig[3]^2
  B <- exp(-(Sig[2] * x^2 - 2 * Sig[3] * x * y + Sig[1] * y^2) / (2 * det)) /
    (2 * pi * sqrt(det))

  # Circular convolution via FFT, shifted so the kernel is centered
  E <- Re(stats::fft(stats::fft(A) * stats::fft(B), inverse = TRUE)) / length(A)
  shift <- c((2 * R + 1):length(d), 1:(2 * R))
  E <- E[shift, shift]

  # Keep aiming locations within the board's bounding square
  keep <- (R + 1):(3 * R + 1)
  E <- E[keep, keep]
  dimnames(E) <- list(-R:R, -R:R)
  E
}

#' Optimal aiming location
#'
#' Finds the aiming location with the highest expected score.
#'
#' @param e Matrix of expected scores, from [simpleExpScores()] or
#'   [generalExpScores()].
#' @return A named numeric vector with the coordinates `x` and `y` of the
#'   best aiming location (in mm, relative to the center of the board), and
#'   the expected `score` when aiming there.
#' @export
#' @examples
#' e <- simpleExpScores(simpleEM(throws)$s.final)
#' aimSpot(e)
aimSpot <- function(e) {
  check_heatmap(e)
  R <- board_radius()
  ij <- arrayInd(which.max(e), dim(e))
  c(x = ij[1] - R - 1, y = ij[2] - R - 1, score = max(e))
}

check_heatmap <- function(e) {
  n <- 2 * board_radius() + 1
  if (!is.matrix(e) || !is.numeric(e) || !all(dim(e) == n)) {
    stop("`e` must be a ", n, " x ", n, " matrix of expected scores, ",
         "from simpleExpScores() or generalExpScores().", call. = FALSE)
  }
  invisible(e)
}
