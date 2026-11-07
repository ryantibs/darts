test_that("plotting functions run without error", {
  pdf(NULL)
  on.exit(dev.off())
  e <- simpleExpScores(400)
  expect_null(drawBoard(new = TRUE))
  expect_null(drawHeatmap(e, board = TRUE, aim = TRUE))
  expect_equal(drawAimSpot(e), aimSpot(e))
})
