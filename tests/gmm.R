library(quanteda)
library(wordvector)
library(embTM)

corp <- wordvector::data_corpus_news2014

toks_test <- tokens(corp, remove_punct = TRUE,
                    remove_symbols = TRUE, remove_number = TRUE) |>
             tokens_remove(stopwords("en"), min_nchar = 2) |>
             tokens_subset(min_ntoken = 2)

wov_test <- textmodel_word2vec(toks_test, dim = 100, min_count = 5)
dfmt_test <- head(dfm(toks_test, remove_padding = TRUE), 2000)
dov_test <- as.textmodel_doc2vec(dfmt_test, wov_test)

test_that("textmodel_gmm works", {

  set.seed(1234)

  mat1 <- head(as.matrix(dov_test, normalize = FALSE), 1000)
  gmm1 <- textmodel_gmm(mat1, k = 10, verbose = FALSE)

  expect_true(
    is.factor(topics(gmm1))
  )
  expect_equal(
    levels(topics(gmm1)),
    paste0("topic", 1:10)
  )
  expect_equal(
    dim(posterior(gmm1)),
    c(1000, 10)
  )

  # use model
  mat2 <- tail(as.matrix(dov_test, normalize = FALSE), 1000)
  gmm2 <- textmodel_gmm(mat2, model = gmm1, verbose = FALSE)

  expect_true(
    is.factor(topics(gmm2))
  )
  expect_equal(
    levels(topics(gmm2)),
    paste0("topic", 1:10)
  )
  expect_equal(
    dim(posterior(gmm2)),
    c(1000, 10)
  )

  term1 <- terms(gmm1, head(dfmt_test, 1000), n = 20)
  term2 <- terms(gmm2, tail(dfmt_test, 1000), n = 20)
  expect_true(
    all(sapply(1:10, function(i) length(intersect(term1[,i], term2[,i]))) > 1),
  )

  # use cluster
  mat3 <- tail(as.matrix(dov_test, normalize = FALSE), 1000)
  gmm3 <- textmodel_gmm(mat3, cluster = posterior(gmm1, newdata = mat3),
                        verbose = FALSE)
  expect_true(
    is.factor(topics(gmm3))
  )
  expect_equal(
    levels(topics(gmm3)),
    paste0("topic", 1:10)
  )
  expect_equal(
    dim(posterior(gmm3)),
    c(1000, 10)
  )

  term3 <- terms(gmm3, tail(dfmt_test, 1000), n = 20)
  expect_true(
    all(sapply(1:10, function(i) length(intersect(term1[,i], term3[,i]))) > 1),
  )
})

