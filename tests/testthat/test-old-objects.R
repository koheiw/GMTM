
test_that("old GMM objects works", {

  dfmt <- readRDS("tests/data/dfm.RDS")
  gmm_v01 <- readRDS("tests/data/gmm_v0.1.0.RDS")

  expect_equal(
    dimnames(terms(gmm_v01, dfmt)),
    list(NULL, paste0("topic", 1:10))
  )
  expect_equal(
    dimnames(probability(gmm_v01)),
    list(gmm_v01$docname, paste0("topic", 1:10))
  )
  expect_equal(
    levels(topics(gmm_v01)),
    paste0("topic", 1:10)
  )
  expect_equal(
    names(topics(gmm_v01)),
    gmm_v01$docname
  )

  gmm_v02 <- readRDS("tests/data/gmm_v0.2.0.RDS")

  expect_equal(
    dimnames(terms(gmm_v02, dfmt)),
    list(NULL, paste0("topic", 1:10))
  )
  expect_equal(
    dimnames(probability(gmm_v02)),
    list(gmm_v02$docname, paste0("topic", 1:10))
  )
  expect_equal(
    levels(topics(gmm_v02)),
    paste0("topic", 1:10)
  )
  expect_equal(
    names(topics(gmm_v02)),
    gmm_v02$docname
  )

})

test_that("old kmeans objects works", {

  km_v01 <- readRDS("tests/data/kmeans_v0.1.0.RDS")

  expect_equal(
    dimnames(terms(km_v01, dfmt)),
    list(NULL, paste0("topic", 1:10))
  )
  expect_equal(
    levels(topics(km_v01)),
    paste0("topic", 1:10)
  )
  expect_equal(
    names(topics(km_v01)),
    km_v01$docname
  )

  km_v02 <- readRDS("tests/data/kmeans_v0.2.0.RDS")

  expect_equal(
    dimnames(terms(km_v02, dfmt)),
    list(NULL, paste0("topic", 1:10))
  )
  expect_equal(
    levels(topics(km_v02)),
    paste0("topic", 1:10)
  )
  expect_equal(
    names(topics(km_v02)),
    km_v02$docname
  )

})
