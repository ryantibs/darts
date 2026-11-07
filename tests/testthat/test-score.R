test_that("score() handles each region of the board", {
  expect_equal(score(0, 0), 50L)
  expect_equal(score(10, 0), 25L)
  expect_equal(score(0, 50), 20L)    # inner single 20
  expect_equal(score(0, 103), 60L)   # triple 20
  expect_equal(score(0, 130), 20L)   # outer single 20
  expect_equal(score(0, 166), 40L)   # double 20
  expect_equal(score(0, 171), 0L)    # miss
  expect_equal(score(0, -103), 9L)   # triple 3, at the bottom
  expect_equal(score(103, 0), 18L)   # triple 6, on the right
  expect_equal(score(-166, 0), 22L)  # double 11, on the left
})

test_that("score() is vectorized and propagates NA", {
  expect_equal(score(c(0, 0, NA), c(0, 103, 0)), c(50L, 60L, NA))
  expect_error(score(1:2, 1), "same length")
  expect_error(score("a", 1), "numeric")
})

test_that("score matrix matches score() on the grid", {
  A <- score_matrix()
  expect_equal(dim(A), c(681L, 681L))
  pts <- cbind(c(-340, 0, 0, 25, -120, 340), c(-340, 0, 103, -150, 40, 0))
  expect_equal(A[pts + 341], score(pts[, 1], pts[, 2]))
  expect_setequal(unique(as.vector(A)), possible_scores())
})

test_that("impossible scores are rejected", {
  expect_error(simpleEM(c(20, 23)), "impossible scores: 23")
  expect_error(simpleEM(c(20, NA)), "missing")
  expect_error(simpleEM(numeric()), "non-empty")
})
