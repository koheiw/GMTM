library(quanteda)
library(wordvector)
library(GMTM)
options(wordvector_threads = 2)
options(GMTM.threads = 2)

corp <- wordvector::data_corpus_news2014

toks_test <- tokens(corp, remove_punct = TRUE,
                    remove_symbols = TRUE, remove_number = TRUE) |>
             tokens_remove(stopwords("en"), min_nchar = 2) |>
             tokens_subset(min_ntoken = 2)

wov_test <- textmodel_word2vec(toks_test, dim = 100, min_count = 5)
dfmt_test <- head(dfm(toks_test, remove_padding = TRUE), 2000)
dov_test <- as.textmodel_doc2vec(dfmt_test, wov_test)
gmm_test <- textmodel_gmm(dov_test)

test_that("textmodel_gmm works", {

  expect_equal(
    names(gmm_test),
    c("k", "centers", "covariance", "likelihood", "cluster", "label",
      "docname", "call", "version")
  )
  expect_equal(
    names(topics(gmm_test)),
    rownames(dfmt_test),
  )
  expect_true(
    is.factor(topics(gmm_test))
  )
  expect_equal(
    levels(topics(gmm_test)),
    paste0("topic", 1:10)
  )
  expect_equal(
    dim(terms(gmm_test, dfmt_test, 15)),
    c(15, 10)
  )
  expect_output(
    print(gmm_test),
    "Call:\ntextmodel_gmm\\(.*\\)"
  )
  expect_error(
    terms(gmm_test, head(dfmt_test, 100), n = 20),
    "the number of documents do not match"
  )

})

test_that("model works", {

  skip_on_cran()

  set.seed(1234)
  gmm1 <- textmodel_gmm(dov_test, k = 15, verbose = FALSE)
  expect_message(
    gmm2 <- textmodel_gmm(dov_test, model = gmm1, verbose = FALSE),
    "k is overwritten by the fitted model"
  )
  term1 <- terms(gmm1, dfmt_test, n = 10)
  term2 <- terms(gmm2, dfmt_test, n = 10)

  expect_true(
    all(sapply(1:15, function(i) length(intersect(term1[,i], term2[,i]))) > 0),
  )

})

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
    as.seedwords(dict, dov_test),
    "model must be a trained textmodel_word2vec"
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

test_that("seeds works", {

  dict <- dictionary(list(eco = "econom*", sec = "securit*", spo = "sport*",
                          pol = "politi*", cri = "crime"))

  seed1 <- as.seedwords(dict, wov_test, residual = 0)
  gmm1 <- textmodel_gmm(dov_test, seeds = seed1)
  expect_equal(
    colnames(terms(gmm1, dfmt_test)),
    names(dict)
  )

  seed2 <- as.seedwords(dict, wov_test, residual = 1)
  gmm2 <- textmodel_gmm(dov_test, seeds = seed2)
  expect_equal(
    colnames(terms(gmm2, dfmt_test)),
    c(names(dict), "other")
  )

  expect_error(
    textmodel_gmm(dov_test, model = gmm1, seeds = seed1),
    "either model or seeds must be NULL"
  )

})

