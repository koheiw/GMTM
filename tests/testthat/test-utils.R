
test_that("get_threads works", {

  options(GMTM.threads = 3)
  expect_equal(
    GMTM:::get_threads(), 3
  )

  options(GMTM.threads = "xxx")
  expect_error(
    GMTM:::get_threads(),
    "GMTM.threads must be an integer",
    fixed = TRUE
  )

  # restore
  options(GMTM.threads = 2)

})

test_that("OMP is enabled", {

  skip_on_os("mac")

  expect_true(
    GMTM:::cpp_omp_enabled()
  )

})

test_that("OMP is enabled", {

  mat <- matrix(rep(c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0), 5), nrow = 7, ncol = 5)
  rownames(mat) <- paste0("doc", c(2, 2, 2, 1, 1, 3, 0))

  expect_equal(
    group_matrix(mat, rownames(mat)),
    matrix(c(0, 0.4, 0.6, 0.2), nrow = 4, ncol = 5,
           dimnames = list(paste0("doc", 0:3), NULL))
  )

  expect_equal(
    group_matrix(mat, rownames(mat), normalize = FALSE),
    matrix(c(0, 0.2, 0.3, 0.1), nrow = 4, ncol = 5,
           dimnames = list(paste0("doc", 0:3), NULL))
  )

  v1 <- factor(rownames(mat), levels = paste0("doc", 0:4))
  expect_equal(
    group_matrix(mat, v1),
    matrix(c(0, 0.4, 0.6, 0.2, 0), nrow = 5, ncol = 5,
           dimnames = list(paste0("doc", 0:4), NULL))
  )

  v2 <- factor(rownames(mat), levels = paste0("doc", 4:0))
  expect_equal(
    group_matrix(mat, v2),
    matrix(c(0, 0.2, 0.6, 0.4, 0), nrow = 5, ncol = 5,
           dimnames = list(paste0("doc", 4:0), NULL))
  )

  v3 <- paste0("doc", c(2, 2, 2, 1, 1, 3, 0, 0))
  expect_error(
    group_matrix(mat, v3),
    "the length of factor does not much nrow(x)",
    fixed = TRUE
  )

})
