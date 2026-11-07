# Simulate scores of n throws aimed at the center, with Gaussian errors
simulate_throws <- function(n, Sig) {
  L <- chol(matrix(c(Sig[1], Sig[3], Sig[3], Sig[2]), 2))
  z <- matrix(stats::rnorm(2 * n), n) %*% L
  score(z[, 1], z[, 2])
}
