
test_that("get_threads works", {

  withr::local_options(list(GMTM.threads = 3))
  expect_equal(
    GMTM:::get_threads(), 3
  )

  withr::local_options(list(GMTM.threads = "xxx"))
  expect_error(
    GMTM:::get_threads(),
    "GMTM.threads must be an integer",
    fixed = TRUE
  )

})

test_that("OMP is enabled", {

  skip_on_cran()
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

  expect_error(
    group_matrix(list(), v2),
    "x must be a matrix",
  )

  v3 <- paste0("doc", c(2, 2, 2, 1, 1, 3, 0, 0))
  expect_error(
    group_matrix(mat, v3),
    "The length of the factor does not much nrow(x)",
    fixed = TRUE
  )

})

test_that("upgrade_gmm is working", {

  gmm_v01 <- upgrade_gmm(readRDS("tests/data/gmm_v0.1.0.RDS"))
  expect_equal(
    dim(gmm_v01$topic.likelihood),
    c(1000, 10)
  )
  expect_setequal(gmm_v01$topic, 1L:10L)
  expect_equal(length(gmm_v01$topic), 1000)
  expect_equal(
    gmm_v01$docvars$docname_,
    names(head(wordvector::data_corpus_news2014, 1000))
  )

  gmm_v02 <- upgrade_gmm(readRDS("tests/data/gmm_v0.2.0.RDS"))
  expect_equal(
    dim(gmm_v02$topic.likelihood),
    c(1000, 10)
  )
  expect_setequal(gmm_v02$topic, 1L:10L)
  expect_equal(length(gmm_v02$topic), 1000)
  expect_equal(
    gmm_v02$docvars$docname_,
    names(head(wordvector::data_corpus_news2014, 1000))
  )

})

