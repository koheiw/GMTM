library(quanteda)
library(wordvector)
library(GMTM)
options(wordvector_threads = 2)
options(GMTM.threads = 2)

corp <- wordvector::data_corpus_news2014
corp_test <- corpus_reshape(corp)

toks_test <- tokens(corp_test, remove_punct = TRUE,
                    remove_symbols = TRUE, remove_numbers = TRUE) |>
  tokens_remove(stopwords("en"), min_nchar = 2) |>
  tokens_subset(min_ntoken = 2)
wov_test <- textmodel_word2vec(toks_test, dim = 100, min_count = 2)

dfmt_test <- dfm(toks_test[1:5000], remove_padding = TRUE)
dov_test <- as.textmodel_doc2vec(dfmt_test, wov_test)
gmm_test <- textmodel_gmm(dov_test)

test_that("as.seedwords works", {

  dict <- dictionary(list(eco = "econom*", sec = "securit*", spo = "sport*",
                          pol = "politi*", cri = c("crime", "police officer*"),
                          xxx = "xxx"))

  seed1 <- as.seedwords(dict, wov_test)
  expect_equal(
    dim(seed1),
    c(6, 100)
  )
  expect_equal(
    apply(seed1, 1, function(x) all(x == 0)),
    c("eco" = FALSE,
      "sec" = FALSE,
      "spo" = FALSE,
      "pol" = FALSE,
      "cri" = FALSE,
      "xxx" = TRUE)
  )

  seed2 <- as.seedwords(dict, wov_test, residual = 1)
  expect_equal(
    dim(seed2),
    c(7, 100)
  )
  expect_equal(
    apply(seed2, 1, function(x) all(x == 0)),
    c("eco" = FALSE,
      "sec" = FALSE,
      "spo" = FALSE,
      "pol" = FALSE,
      "cri" = FALSE,
      "xxx" = TRUE,
      "other"= FALSE)
  )

  seed3 <- as.seedwords(dict, wov_test, residual = 2)
  expect_equal(
    dim(seed3),
    c(8, 100)
  )
  expect_equal(
    apply(seed3, 1, function(x) all(x == 0)),
    c("eco" = FALSE,
      "sec" = FALSE,
      "spo" = FALSE,
      "pol" = FALSE,
      "cri" = FALSE,
      "xxx" = TRUE,
      "other1"= FALSE,
      "other2"= FALSE)
  )

  options(GMTM.residual.name = "else")
  seed4 <- as.seedwords(dict, wov_test, residual = 1)
  expect_equal(
    dim(seed2),
    c(7, 100)
  )
  expect_equal(
    apply(seed4, 1, function(x) all(x == 0)),
    c("eco" = FALSE,
      "sec" = FALSE,
      "spo" = FALSE,
      "pol" = FALSE,
      "cri" = FALSE,
      "xxx" = TRUE,
      "else"= FALSE)
  )
  options(GMTM.residual.name = "other") # restore

  expect_error(
    as.seedwords(dict, list()),
    "model does not have the layer for words"
  )

  expect_error(
    as.seedwords(list(), wov_test),
    "x must be a dictionary object"
  )

  expect_error(
    as.seedwords(dict, wov_test, residual = -1),
    "The value of residual must be between 0 and Inf"
  )

})

test_that("as.seedwords works with hierarchical dictionary", {

  dict <- dictionary(file = "../data/newsmap.yml")

  seed1 <- as.seedwords(dict, wov_test)
  expect_equal(
    dim(seed1),
    c(5, 100)
  )
  expect_all_true(
    rowSums(seed1) != 0
  )

  seed2 <- as.seedwords(dict, wov_test, levels = 1:2)
  expect_equal(
    dim(seed2),
    c(22, 100)
  )
  expect_all_true(
    rowSums(seed2) != 0
  )

})

# test_that("as.seedwords works", {
#
#   dict <- dictionary(list(eco = "econom*", sec = "securit*", spo = "sport*",
#                           pol = "politi*", cri = "crime"))
#
#   seed1 <- as.seedwords(dict, wov_test, residual = 0)
#   gmm1 <- textmodel_gmm(dov_test, seeds = seed1)
#   expect_equal(
#     colnames(terms(gmm1, dfmt_test)),
#     names(dict)
#   )
#
#   seed2 <- as.seedwords(dict, wov_test, residual = 1)
#   gmm2 <- textmodel_gmm(dov_test, seeds = seed2)
#   expect_equal(
#     colnames(terms(gmm2, dfmt_test)),
#     c(names(dict), "other")
#   )
#
#   expect_error(
#     textmodel_gmm(dov_test, model = gmm1, seeds = seed1),
#     "either the model or seeds must be NULL"
#   )
#
# })

test_that("as.seedwords works with handle multi-word expressions", {

  dict <- dictionary(list(US = c("united states", "us", "america"),
                          JP = "tokyo"))

  mat1 <- rbind(united = c(1, 0, 1, 0),
                states = c(0, 1, 0, 1))
  expect_equal(
    as.seedwords(dict, as.textmodel_word2vec(mat1)),
    rbind(US = c(1, 1, 1, 1),
          JP = c(0, 0, 0, 0))
  )

  mat2 <- rbind("united_states" = c(1, 1, 1, 1))
  expect_equal(
    as.seedwords(dict, as.textmodel_word2vec(mat2)),
    rbind(US = c(1, 1, 1, 1),
          JP = c(0, 0, 0, 0))
  )

  mat3 <- rbind("united states" = c(1, 1, 1, 1))
  expect_equal(
    as.seedwords(dict, as.textmodel_word2vec(mat3, concatenator = " ")),
    rbind(US = c(1, 1, 1, 1),
          JP = c(0, 0, 0, 0))
  )

  mat4 <- rbind("xxx" = c(1, 1, 1, 1))
  expect_equal(
    as.seedwords(dict, as.textmodel_word2vec(mat4)),
    rbind(US = c(0, 0, 0, 0),
          JP = c(0, 0, 0, 0))
  )
})

