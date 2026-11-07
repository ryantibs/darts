# Scores of 100 throws by Andrew Price aimed at the center of the board, from
# the original (2011) version of the package
throws <- c(12L, 16L, 19L, 3L, 17L, 1L, 25L, 19L, 17L, 50L, 18L, 1L, 3L, 17L,
            2L, 2L, 13L, 18L, 16L, 2L, 25L, 5L, 5L, 1L, 5L, 4L, 17L, 25L, 25L,
            50L, 3L, 7L, 17L, 17L, 3L, 3L, 3L, 7L, 11L, 10L, 25L, 1L, 19L, 15L,
            4L, 1L, 5L, 12L, 17L, 16L, 50L, 20L, 20L, 20L, 25L, 50L, 2L, 17L,
            3L, 20L, 20L, 20L, 5L, 1L, 18L, 15L, 2L, 3L, 25L, 12L, 9L, 3L, 3L,
            19L, 16L, 20L, 5L, 5L, 1L, 4L, 15L, 16L, 5L, 20L, 16L, 2L, 25L, 6L,
            12L, 25L, 11L, 25L, 7L, 2L, 5L, 19L, 17L, 17L, 2L, 12L)
stopifnot(length(throws) == 100)
usethis::use_data(throws, overwrite = TRUE)
