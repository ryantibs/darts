test_that("simpleEM increases the log likelihood and converges", {
  fit <- simpleEM(throws)
  expect_named(fit, c("s.final", "s.init", "s", "loglik", "niter"))
  expect_length(fit$s, 100)
  expect_true(all(diff(fit$loglik) >= -1e-8))
  expect_equal(fit$s.final, 799.028, tolerance = 1e-4)
})

test_that("simpleEM recovers the variance of simulated throws", {
  set.seed(1)
  x <- simulate_throws(2000, c(400, 400, 0))
  expect_equal(simpleEM(x)$s.final, 400, tolerance = 0.1)
})

test_that("simpleEM handles misses", {
  set.seed(2)
  x <- simulate_throws(2000, c(80^2, 80^2, 0))
  expect_true(any(x == 0))
  expect_equal(simpleEM(x)$s.final, 80^2, tolerance = 0.1)
})

test_that("generalEM is reproducible given a seed", {
  a <- generalEM(throws, niter = 5, nsamp = 200, seed = 3)
  b <- generalEM(throws, niter = 5, nsamp = 200, seed = 3)
  expect_identical(a, b)
  expect_named(a$Sig.final, c("var_x", "var_y", "cov_xy"))
  expect_equal(dim(a$Sig), c(5L, 3L))
})

test_that("generalEM recovers the covariance of simulated throws", {
  set.seed(4)
  Sig <- c(300, 900, 200)
  x <- simulate_throws(1000, Sig)
  est <- generalEM(x, niter = 40, nsamp = 500)$Sig.final
  expect_equal(unname(est), Sig, tolerance = 0.25)
})

test_that("generalEM handles misses", {
  set.seed(5)
  x <- simulate_throws(500, c(80^2, 60^2, 0))
  expect_true(any(x == 0))
  est <- generalEM(x, niter = 30, nsamp = 500)$Sig.final
  expect_true(all(is.finite(est)))
  expect_equal(unname(est[1:2]), c(80^2, 60^2), tolerance = 0.25)
})

test_that("EM arguments are validated", {
  expect_error(simpleEM(throws, s.init = -1), "positive number")
  expect_error(simpleEM(throws, niter = 0), "positive integer")
  expect_error(generalEM(throws, Sig.init = c(1, 1, 2)), "positive definite")
})
