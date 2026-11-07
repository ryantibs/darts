# Board geometry ---------------------------------------------------------

# Radii (mm) of the wires, from the center outward, and the numbers around
# the board, clockwise from the top.
board_geom <- list(
  R = c(
    6.35, # double bullseye
    15.9, # single bullseye
    99, # inner triple ring
    107, # outer triple ring
    162, # inner double ring
    170 # outer double ring
  ),
  S = c(20L, 1L, 18L, 4L, 13L, 6L, 10L, 15L, 2L, 17L,
        3L, 19L, 7L, 16L, 8L, 11L, 14L, 9L, 12L, 5L)
)

# Outer radius of the board
board_radius <- function() board_geom$R[6]

# All possible scores from a single dart
possible_scores <- function() {
  S <- board_geom$S
  sort(unique(c(0L, S, 2L * S, 3L * S, 25L, 50L)))
}

check_scores <- function(x, arg = "x") {
  if (!is.numeric(x) || length(x) == 0) {
    stop("`", arg, "` must be a non-empty numeric vector of scores.",
         call. = FALSE)
  }
  if (anyNA(x)) {
    stop("`", arg, "` must not contain missing values.", call. = FALSE)
  }
  bad <- setdiff(unique(x), possible_scores())
  if (length(bad) > 0) {
    stop("`", arg, "` contains impossible scores: ",
         paste(sort(bad), collapse = ", "), ".", call. = FALSE)
  }
  as.integer(x)
}

#' Score of a dart
#'
#' Computes the score of a dart landing at a given location on a standard
#' dartboard.
#'
#' @param x,y Coordinates of the dart in millimeters, relative to the center
#'   of the board (positive `y` points toward the 20). Vectors of the same
#'   length.
#' @return An integer vector of scores, with 0 for darts that miss the board.
#' @export
#' @examples
#' score(0, 0)       # double bullseye
#' score(0, 103)     # triple 20
#' score(c(0, 200), c(166, 0))  # double 20, then a miss
score <- function(x, y) {
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("`x` and `y` must be numeric.", call. = FALSE)
  }
  if (length(x) != length(y)) {
    stop("`x` and `y` must have the same length.", call. = FALSE)
  }
  score_cpp(as.double(x), as.double(y), board_geom$R, board_geom$S)
}

# Score at every point of a 1mm grid on [-340, 340]^2, the domain used for
# the FFT convolutions. Cached after the first call.
cache <- new.env(parent = emptyenv())

score_matrix <- function() {
  if (is.null(cache$A)) {
    cache$A <- score_matrix_cpp(2L * board_radius(), board_geom$R,
                                board_geom$S)
  }
  cache$A
}
