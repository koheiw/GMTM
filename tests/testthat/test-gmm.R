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

dfmt_test <- dfm(toks_test, remove_padding = TRUE) |>
  dfm_subset(docid_ %in% head(levels(docid(toks_test)), 1000))
dov_test <- as.textmodel_doc2vec(dfmt_test, wov_test)

test_that("textmodel_gmm works with doc2vec", {

  gmm_dov <- textmodel_gmm(dov_test)

  expect_equal(
    names(gmm_dov),
    c("k", "omit", "centers", "covariance", "topic.likelihood",
      "model", "model.likelihood", "frequency", "label", "docvars",
      "call", "version")
  )
  expect_equal(
    colnames(gmm_dov$docvars),
    c("docname_", "docid_", "segid_", "date")
  )
  expect_equal(
    gmm_dov$frequency,
    dov_test$frequency
  )

  # topics
  expect_equal(
    names(topics(gmm_dov)),
    rownames(dfmt_test),
  )
  expect_true(
    is.factor(topics(gmm_dov))
  )
  expect_equal(
    levels(topics(gmm_dov)),
    paste0("topic", 1:10)
  )
  expect_equal(
    names(topics(gmm_dov)),
    docnames(dfmt_test)
  )
  expect_equal(
    names(topics(gmm_dov, group = gmm_dov$docvars$docid_)),
    levels(docid(dfmt_test))
  )

  # terms
  expect_equal(
    dim(terms(gmm_dov, dfmt_test[1:1000,], 15)),
    c(15, 10)
  )
  expect_equal(
    dim(terms(gmm_dov, toks_test[1:1000], 15)),
    c(15, 10)
  )
  expect_error(
    terms(gmm_dov, tail(toks_test, 10)),
    "data must contain documents on which the model was trained"
  )
  expect_equal(
    dim(terms(topics(gmm_dov), dfmt_test[1:1000,], 15)),
    c(15, 10)
  )
  expect_equal(
    dim(terms(topics(gmm_dov), toks_test[1:1000], 15)),
    c(15, 10)
  )
  expect_error(
    dim(terms(topics(gmm_dov), tail(toks_test, 10), 15)),
    "data must contain documents for which topics were predicted"
  )

  # probability
  prob_ng <- probability(gmm_dov)
  expect_equal(
    dim(prob_ng),
    c(3529, 10)
  )
  expect_equal(
    rownames(prob_ng),
    docnames(dfmt_test)
  )
  expect_true(
    all(round(rowSums(prob_ng), 10) %in% c(1.0, NA_real_))
  )
  prob_gp <- probability(gmm_dov, group = gmm_dov$docvars$docid_)
  expect_equal(
    dim(prob_gp),
    c(1000, 10)
  )
  expect_equal(
    rownames(prob_gp),
    levels(docid(dfmt_test))
  )
  expect_true(
    all(round(rowSums(prob_gp), 10) %in% c(1.0, NA_real_))
  )
  expect_error(
    probability(gmm_dov, head(gmm_dov$docvars$docid_, 10)),
    "The length of group does not much the number of documents"
  )

  # print
  expect_output(
    print(gmm_dov),
    "Call:\ntextmodel_gmm\\(.*\\)"
  )

  # error
  expect_error(
    textmodel_gmm(dov_test$values$doc[1:2,]),
    "Failed to train Gaussian mixture model"
  )

  gmm_temp <- gmm_dov
  gmm_temp$label <- head(gmm_temp$label, 5)
  expect_error(
    topics(gmm_temp),
    "The length of label is invalid"
  )
})

test_that("textmodel_gmm works with matrix", {

  gmm_mat <- textmodel_gmm(as.matrix(dov_test))

  expect_equal(
    names(gmm_mat),
    c("k", "omit", "centers", "covariance", "topic.likelihood",
      "model", "model.likelihood", "frequency", "label", "docvars",
      "call", "version")
  )
  expect_null(
    gmm_mat$docvars
  )
  expect_null(
    gmm_mat$frequency
  )

  # terms
  expect_equal(
    dim(terms(gmm_mat, dfmt_test, 15)),
    c(15, 10)
  )
  # topics
  expect_equal(
    dim(terms(topics(gmm_mat), dfmt_test, 15)),
    c(15, 10)
  )

})

test_that("model works with doc2vec", {

  skip_on_cran()

  withr::local_options(list(GMTM.threads = 1))
  set.seed(1234)

  gmm_dov0 <- textmodel_gmm(dov_test, k = 15, verbose = FALSE)
  expect_message(
    gmm_dov1 <- textmodel_gmm(dov_test, model = gmm_dov0, iter_km = 0, verbose = FALSE),
    "k is overwritten by the fitted model"
  )
  term0 <- terms(gmm_dov0, dfmt_test, n = 10)
  term1 <- terms(gmm_dov1, dfmt_test, n = 10)

  expect_true(
    all(sapply(1:15, function(i) length(intersect(term0[,i], term1[,i]))) > 0),
  )

  expect_error(
    textmodel_gmm(dov_test, model = list()),
    "the model must be a fitted textmodel_gmm"
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

test_that("seeds works", {

  dict <- dictionary(list(eco = "econom*", sec = "securit*", spo = "sport*",
                          pol = "politi*", cri = "crime"))

  seed1 <- as.seedwords(dict, wov_test, residual = 0)
  gmm_dov1 <- textmodel_gmm(dov_test, seeds = seed1)
  expect_equal(
    colnames(terms(gmm_dov1, dfmt_test)),
    names(dict)
  )

  seed2 <- as.seedwords(dict, wov_test, residual = 1)
  gmm_dov2 <- textmodel_gmm(dov_test, seeds = seed2)
  expect_equal(
    colnames(terms(gmm_dov2, dfmt_test)),
    c(names(dict), "other")
  )

  expect_error(
    textmodel_gmm(dov_test, model = gmm_dov1, seeds = seed1),
    "either the model or seeds must be NULL"
  )

})

test_that("returns NA for empty documents", {

  gmm_dov <- textmodel_gmm(dov_test, k = 15, verbose = FALSE)
  b <- rowSums(abs(dov_test$values$doc)) == 0

  expect_true(
    all(is.na(gmm_dov$cluster[b]))
  )
  expect_true(
    all(is.na(gmm_dov$topic.likelihood[b,]))
  )

})

test_that("dist_type works", {

  expect_true(
    GMTM:::is.textmodel_gmm(textmodel_gmm(dov_test, dist_type = 1))
  )
  expect_true(
    GMTM:::is.textmodel_gmm(textmodel_gmm(dov_test, dist_type = 2))
  )

})


test_that("results are reproduced", {

  withr::local_options(list(GMTM.threads = 1))

  mat <- replicate(10, {
    set.seed(1234)
    topics(textmodel_gmm(dov_test))
  })
  expect_true(
    all(mat[,1] == mat)
  )
})


test_that("omit works", {

  gmm1 <- textmodel_gmm(dov_test, omit = 1:3)
  expect_true(
    GMTM:::is.textmodel_gmm(gmm1)
  )

  expect_identical(
    gmm1$omit,
    1L:3L
  )

  expect_error(
    textmodel_gmm(dov_test, omit = -1),
    "The value of omit must be between 1 and 100"
  )

})

test_that("gmm errors with NA", {

  mat <- as.matrix(dov_test)

  mat[1:100,] <- NA
  expect_error(
    textmodel_gmm(mat),
    "x should not contain any NA"
  )

  mat[1:100,] <- 0
  expect_identical(
    class(textmodel_gmm(mat)),
    c("textmodel_gmm", "textmodel_gmtm")
  )

})

