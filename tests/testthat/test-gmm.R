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

test_that("textmodel_gmm works with doc2vec", {

  expect_equal(
    names(gmm_test),
    c("k", "centers", "topic", "covariance", "topic.likelihood",
      "model.likelihood", "frequency", "label", "docvars",
      "call", "version")
  )
  expect_equal(
    colnames(gmm_test$docvars),
    c("docname_", "docid_", "segid_", "date")
  )
  expect_equal(
    gmm_test$frequency,
    dov_test$frequency
  )

  # topics
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
    names(topics(gmm_test)),
    docnames(dfmt_test)
  )
  expect_equal(
    names(topics(gmm_test, group = gmm_test$docvars$docid_)),
    levels(docid(dfmt_test))
  )

  # terms
  expect_equal(
    dim(terms(gmm_test, dfmt_test[1:1000,], 15)),
    c(15, 10)
  )
  expect_equal(
    dim(terms(gmm_test, toks_test[1:1000,], 15)),
    c(15, 10)
  )
  expect_identical(
    dimnames(terms(gmm_test, dfmt_test[1:1000,], 15)),
    list(NULL, paste0("topic", 1:10))
  )
  expect_silent(
    terms(gmm_test, dfm_tfidf(dfmt_test))
  )
  expect_error(
    terms(gmm_test, dfmt_test[1:1000,], filter = "government"),
    "filter is defunct in v0.3.0; use dfm_select(data, pattern = filter) instead",
    fixed = TRUE
  )
  expect_error(
    terms(gmm_test, toks_test[1:1000], filter = "government"),
    "filter is defunct in v0.3.0; use tokens_select(data, pattern = filter) instead",
    fixed = TRUE
  )
  expect_error(
    terms(gmm_test, tail(toks_test, 10)),
    "data must contain documents on which the model was trained"
  )

  expect_equal(
    dim(terms(topics(gmm_test), dfmt_test[1:1000,], 15)),
    c(15, 10)
  )
  expect_equal(
    dim(terms(topics(gmm_test), dfmt_test[1:1000,], 15)),
    c(15, 10)
  )
  expect_error(
    terms(topics(gmm_test), dfmt_test[1:1000,], filter = "government"),
    "filter is defunct in v0.3.0; use dfm_select(data, pattern = filter) instead",
    fixed = TRUE
  )
  expect_error(
    terms(topics(gmm_test), toks_test[1:1000], filter = "government"),
    "filter is defunct in v0.3.0; use tokens_select(data, pattern = filter) instead",
    fixed = TRUE
  )
  expect_error(
    dim(terms(topics(gmm_test), tail(toks_test, 10), 15)),
    "data must contain documents for which topics were predicted"
  )

  # probability
  prob_ng <- probability(gmm_test)
  expect_equal(
    dim(prob_ng),
    c(5000, 10)
  )
  expect_equal(
    rownames(prob_ng),
    docnames(dfmt_test)
  )
  expect_true(
    all(round(rowSums(prob_ng), 10) %in% c(1.0, NA_real_))
  )
  prob_gp <- probability(gmm_test, group = gmm_test$docvars$docid_)
  expect_equal(
    dim(prob_gp),
    c(1521, 10)
  )
  expect_equal(
    rownames(prob_gp),
    levels(docid(dfmt_test))
  )
  expect_true(
    all(round(rowSums(prob_gp), 10) %in% c(1.0, NA_real_))
  )
  expect_error(
    probability(gmm_test, head(gmm_test$docvars$docid_, 10)),
    "The length of group does not much the number of documents"
  )

  # print
  expect_output(
    print(gmm_test),
    "Call:\ntextmodel_gmm\\(.*\\)"
  )

  # error
  expect_error(
    textmodel_gmm(dov_test$values$doc[1:2,]),
    "Failed to train Gaussian mixture model"
  )

  gmm_temp <- gmm_test
  gmm_temp$label <- head(gmm_temp$label, 5)
  expect_error(
    topics(gmm_temp),
    "The length of label is invalid"
  )
})

test_that("textmodel_gmm works with matrix", {

  mat <- as.matrix(dov_test)
  gmm_mat <- textmodel_gmm(mat)

  expect_equal(
    names(gmm_mat),
    c("k", "centers", "topic", "covariance", "topic.likelihood",
      "model.likelihood", "frequency", "label", "docvars",
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
  skip_on_os("mac")

  withr::local_options(list(GMTM.threads = 1))
  set.seed(1234)

  gmm0 <- textmodel_gmm(dov_test, k = 15, verbose = FALSE)
  expect_message(
    gmm1 <- textmodel_gmm(dov_test, model = gmm0, iter_km = 0, verbose = FALSE),
    "k is overwritten by the fitted model"
  )
  term0 <- terms(gmm0, dfmt_test, n = 10)
  term1 <- terms(gmm1, dfmt_test, n = 10)

  expect_true(
    all(sapply(1:15, function(i) length(intersect(term0[,i], term1[,i]))) > 0),
  )

  expect_error(
    textmodel_gmm(dov_test, model = list()),
    "the model must be a fitted textmodel_gmm"
  )

})


test_that("returns NA for empty documents", {

  b <- rowSums(abs(dov_test$values$doc)) == 0

  expect_true(
    all(is.na(gmm_test$cluster[b]))
  )
  expect_true(
    all(is.na(gmm_test$topic.likelihood[b,]))
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
    all(mat[,1] == mat, na.rm = TRUE)
  )
})


test_that("omit works", {

  gmm1 <- textmodel_gmm(dov_test, omit = 1:3)
  expect_true(
    GMTM:::is.textmodel_gmm(gmm1)
  )

  expect_null(
    gmm1$omit
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

test_that("terms works with various data", {

  toks_pad <- tokens(corp_test[1:1000], remove_punct = TRUE,
                     remove_symbols = TRUE, remove_numbers = TRUE) |>
              tokens_remove(stopwords("en"), min_nchar = 2, padding = TRUE)

  term1 <- terms(gmm_test, toks_pad)
  expect_true(
    is.matrix(term1)
  )
  expect_true(
    any(term1 == "AP")
  )
  expect_true(
    all(term1 != "")
  )

  term2 <- terms(gmm_test, dfm(toks_pad, tolower = FALSE,
                               remove_padding = FALSE))
  expect_true(
    is.matrix(term2)
  )
  expect_true(
    any(term2 == "AP")
  )
  expect_true(
    all(term2 != "")
  )

  term3 <- terms(gmm_test, dfm(toks_pad, tolower = TRUE,
                               remove_padding = FALSE))
  expect_true(
    is.matrix(term3)
  )
  expect_true(
    any(term3 == "ap")
  )
  expect_true(
    all(term3 != "")
  )

  term4 <- terms(gmm_test, dfm(toks_pad[4,]))
  expect_true(
    is.matrix(term4)
  )
  expect_true(
    any(term4 == "ap")
  )
  expect_true(
    all(term4 != "")
  )

  term6 <- terms(topics(gmm_test), dfm(toks_pad, tolower = FALSE,
                                       remove_padding = FALSE))
  expect_true(
    is.matrix(term6)
  )
  expect_true(
    any(term6 == "Ukraine")
  )
  expect_true(
    all(term6 != "")
  )

  expect_error(
    terms(gmm_test, dfm(toks_pad[0,])),
    "data must contain documents on which the model was trained"
  )

  expect_error(
    terms(gmm_test, list()),
    "dfm() only works on dfm, tokens, tokens_xptr objects.",
    fixed = TRUE
  )
})


test_that("get_probability is working", {

  prob1 <- GMTM:::get_probability(gmm_test)
  expect_equal(
    rownames(prob1),
    gmm_test$docvars$docname_
  )
  expect_equal(
    dim(prob1),
    c(5000, 10)
  )

  prob2 <- GMTM:::get_probability(gmm_test, group = gmm_test$docvars$docid_)
  expect_equal(
    rownames(prob2),
    levels(gmm_test$docvars$docid_)
  )
  expect_equal(
    dim(prob2),
    c(1521, 10)
  )

})

test_that("get_topics is working", {

  topic1 <- GMTM:::get_topics(gmm_test)
  expect_equal(length(topic1), 5000)
  expect_true(is.factor(topic1))

  topic2 <- GMTM:::get_topics(gmm_test, group = gmm_test$docvars$docid_)
  expect_equal(length(topic2), 1521)
  expect_true(is.factor(topic2))

})

test_that("include_prob is working", {

  gmm0 <- textmodel_gmm(dov_test, include_prob = TRUE)
  expect_equal(
    dimnames(gmm0$topic.likelihood),
    list(names(gmm0$topic), NULL)
  )
  expect_error(
    textmodel_gmm(dov_test, include_prob = c(TRUE, FALSE)),
    "The length of include_prob must be 1"
  )

  gmm1 <- textmodel_gmm(dov_test, include_prob = FALSE)

  expect_null(gmm1$topic.likelihood)
  expect_equal(
    levels(topics(gmm1)),
    paste0("topic", 1:10)
  )
  expect_error(
    probability(gmm1),
    "The model does not include topic.likelihood"
  )
  expect_error(
    terms(gmm1, dfmt_test),
    "Use terms(topics(x), data) when x is fitted with include_prob = FALSE",
    fixed = TRUE
  )

})
