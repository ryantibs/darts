#' EM algorithm for the simple model
#'
#' Estimates the spread of your throws from the scores of darts aimed at the
#' center of the board. The simple model assumes each throw lands at a
#' bivariate Gaussian location centered at the bullseye, with independent
#' coordinates that each have variance `s`.
#'
#' @param x Scores of throws aimed at the center of the dartboard. Use 0 for
#'   darts that missed the board.
#' @param s.init Initial guess for the variance, in squared millimeters.
#' @param niter Number of EM iterations.
#' @return A list with components:
#'   \item{s.final}{Final estimate of the variance.}
#'   \item{s.init}{Initial guess for the variance.}
#'   \item{s}{Estimate of the variance at each iteration.}
#'   \item{loglik}{Observed-data log likelihood at each iteration.}
#'   \item{niter}{Number of iterations.}
#' @seealso [generalEM()] for a full covariance model, and
#'   [simpleExpScores()] to turn the estimate into a heatmap.
#' @export
#' @examples
#' fit <- simpleEM(throws)
#' fit$s.final
#' sqrt(fit$s.final)  # standard deviation, in mm
#'
#' # Check that the log likelihood has converged
#' plot(fit$loglik, type = "l", xlab = "Iteration", ylab = "Log likelihood")
simpleEM <- function(x, s.init = 100, niter = 100) {
  x <- check_scores(x)
  check_positive(s.init, "s.init")
  niter <- check_count(niter, "niter")

  out <- simple_em_cpp(x, s.init, niter, board_geom$R, board_geom$S)
  list(
    s.final = out$s[niter],
    s.init = s.init,
    s = out$s,
    loglik = out$loglik,
    niter = niter
  )
}

#' EM algorithm for the general model
#'
#' Estimates the spread of your throws from the scores of darts aimed at the
#' center of the board. The general model assumes each throw lands at a
#' bivariate Gaussian location centered at the bullseye, with an arbitrary
#' covariance matrix. The E-step has no closed form, so it is computed by
#' importance sampling; the results are therefore random, and depend on the
#' state of R's random number generator.
#'
#' @inheritParams simpleEM
#' @param Sig.init Initial guess for the covariance matrix, as a vector
#'   `c(var_x, var_y, cov_xy)`, in squared millimeters.
#' @param nsamp Number of importance samples per throw in each E-step.
#' @param seed Optional seed, passed to [set.seed()] before running.
#' @return A list with components:
#'   \item{Sig.final}{Final estimate of the covariance, as
#'     `c(var_x, var_y, cov_xy)`.}
#'   \item{Sig.init}{Initial guess for the covariance.}
#'   \item{Sig}{Matrix with the estimate at each iteration in its rows.}
#'   \item{niter}{Number of iterations.}
#' @seealso [simpleEM()] for the simpler model, and [generalExpScores()] to
#'   turn the estimate into a heatmap.
#' @export
#' @examples
#' fit <- generalEM(throws, niter = 30, seed = 0)
#' fit$Sig.final
#'
#' # Trace of the estimates across iterations
#' matplot(fit$Sig, type = "l", lty = 1, xlab = "Iteration", ylab = "Estimate")
generalEM <- function(x, Sig.init = c(100, 100, 10), niter = 100,
                      nsamp = 1000, seed = NULL) {
  x <- check_scores(x)
  check_cov(Sig.init, "Sig.init")
  niter <- check_count(niter, "niter")
  nsamp <- check_count(nsamp, "nsamp")
  if (!is.null(seed)) set.seed(seed)

  Sig <- general_em_cpp(x, as.double(Sig.init), niter, nsamp,
                        board_geom$R, board_geom$S)
  colnames(Sig) <- c("var_x", "var_y", "cov_xy")
  list(
    Sig.final = Sig[niter, ],
    Sig.init = Sig.init,
    Sig = Sig,
    niter = niter
  )
}

# Argument checks --------------------------------------------------------

check_positive <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x <= 0) {
    stop("`", arg, "` must be a single positive number.", call. = FALSE)
  }
  invisible(x)
}

check_count <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 1 ||
      x != round(x)) {
    stop("`", arg, "` must be a single positive integer.", call. = FALSE)
  }
  as.integer(x)
}

check_cov <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 3 || anyNA(x) ||
      x[1] <= 0 || x[2] <= 0 || x[1] * x[2] - x[3]^2 <= 0) {
    stop("`", arg, "` must be a vector c(var_x, var_y, cov_xy) describing ",
         "a positive definite covariance matrix.", call. = FALSE)
  }
  invisible(x)
}
