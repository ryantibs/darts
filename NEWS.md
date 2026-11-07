# darts 2.0.0

A modernized rewrite of the 2011 package.

## Infrastructure

* Documentation is generated with roxygen2, and the compiled code is written
  in C++ with Rcpp, with registered native routines.
* Added a testthat test suite and a GitHub Actions R CMD check workflow.

## New features

* `score()` computes the score of darts at given board locations.
* `aimSpot()` returns the optimal aiming location and its expected score.
* `drawHeatmap()` gains `board` and `aim` arguments to overlay the board and
  the optimal aiming spot in one call.
* `generalEM()` gains an `nsamp` argument for the number of importance
  samples per throw (previously fixed at 1000).
* The example scores are now included as the dataset `throws`.
* The score matrix used by the expected-score functions is computed once per
  session and cached.

## Bug fixes

* The optimal aiming location was off by 1mm in each direction.
* `generalEM()` now handles misses (scores of 0), which previously used
  uninitialized memory in the E-step.
* `generalEM()` computes importance weights on the log scale, so it no longer
  returns `NaN` when all weights underflow.
* Scores that are impossible on a dartboard (e.g. 23) now give an informative
  error instead of being silently treated as misses.

## Breaking changes

* `generalEM()` no longer returns `loglik`, which was never implemented and
  was always a vector of zeros.
* The plotting functions no longer change `par("mar")`; set margins yourself
  if you want an edge-to-edge plot.
