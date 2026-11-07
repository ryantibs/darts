#' @keywords internal
#' @details
#' A typical analysis has two steps:
#'
#' 1. Throw a set of darts aimed at the center of the board, record the
#'    scores, and estimate the spread of your throws with [simpleEM()]
#'    (a single variance) or [generalEM()] (a full covariance matrix).
#' 2. Compute the expected score for every aiming location with
#'    [simpleExpScores()] or [generalExpScores()], plot it with
#'    [drawHeatmap()], and find the best spot with [aimSpot()].
#'
#' @references
#' Tibshirani, R. J., Price, A., and Taylor, J. (2011). A statistician plays
#' darts. *Journal of the Royal Statistical Society: Series A*, 174(1),
#' 213--226. \doi{10.1111/j.1467-985X.2010.00651.x}
"_PACKAGE"

## usethis namespace: start
#' @useDynLib darts, .registration = TRUE
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL
