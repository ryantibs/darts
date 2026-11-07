test_that("expected scores agree with a direct convolution", {
  s <- 500
  e <- simpleExpScores(s)
  expect_equal(dim(e), c(341L, 341L))
  expect_equal(rownames(e)[c(1, 171, 341)], c("-170", "0", "170"))

  A <- score_matrix()
  d <- -340:340
  k <- stats::dnorm(d, sd = sqrt(s))
  for (p in list(c(0, 0), c(0, 103), c(-30, 80))) {
    i <- p[1] + 341 + d
    j <- p[2] + 341 + d
    ok1 <- i >= 1 & i <= 681
    ok2 <- j >= 1 & j <= 681
    direct <- sum(A[i[ok1], j[ok2]] * outer(k[ok1], k[ok2]))
    expect_equal(e[p[1] + 171, p[2] + 171], direct, tolerance = 1e-8)
  }
})

test_that("simple and general expected scores agree", {
  expect_equal(simpleExpScores(300), generalExpScores(c(300, 300, 0)))
})

test_that("an accurate player should aim at the triple 20", {
  a <- aimSpot(simpleExpScores(5^2))
  expect_equal(score(a["x"], a["y"]), 60L)
  expect_gt(a["score"], 40)
})

test_that("an inaccurate player should aim near the center", {
  a <- aimSpot(simpleExpScores(100^2))
  expect_lt(sqrt(a["x"]^2 + a["y"]^2), 50)
})

test_that("aimSpot() checks its input", {
  expect_error(aimSpot(matrix(0, 3, 3)), "341 x 341")
})
