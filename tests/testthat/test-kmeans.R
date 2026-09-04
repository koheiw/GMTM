library(quanteda)
library(wordvector)
library(EBTM)

corp <- wordvector::data_corpus_news2014

toks_test <- tokens(corp, remove_punct = TRUE,
                    remove_symbols = TRUE, remove_number = TRUE) |>
             tokens_remove(stopwords("en"), min_nchar = 2) |>
             tokens_subset(min_ntoken = 2)

wov_test <- textmodel_word2vec(toks_test, dim = 100, min_count = 5)
dfmt_test <- head(dfm(toks_test, remove_padding = TRUE), 2000)
dov_test <- as.textmodel_doc2vec(dfmt_test, wov_test)

test_that("textmodel_kmeans works", {

  set.seed(1234)

  # use k
  mat1 <- head(as.matrix(dov_test, normalize = FALSE), 1000)
  km1 <- textmodel_kmeans(mat1, k = 10, verbose = FALSE)

  expect_equal(
    names(km1),
    c("kmeans", "data", "k", "label", "docname")
  )
  expect_equal(
    names(topics(km1)),
    rownames(mat1),
  )
  expect_true(
    is.factor(topics(km1))
  )
  expect_equal(
    levels(topics(km1)),
    paste0("topic", 1:10)
  )
  expect_equal(
    dim(predict(km1)),
    c(1000, 100)
  )
  expect_equal(
    dimnames(predict(km1)),
    list(rownames(mat1),
         NULL)
  )
  expect_equal(
    dim(predict(km1, newdata = mat1[1:5,])),
    c(5, 100)
  )

  # use model
  mat2 <- tail(as.matrix(dov_test, normalize = FALSE), 1000)
  km2 <- textmodel_kmeans(mat2, model = km1, verbose = FALSE)

  expect_true(
    is.factor(topics(km2))
  )
  expect_equal(
    levels(topics(km2)),
    paste0("topic", 1:10)
  )
  expect_equal(
    dim(predict(km2)),
    c(1000, 100)
  )

  term1 <- terms(km1, head(dfmt_test, 1000), n = 20)
  term2 <- terms(km2, tail(dfmt_test, 1000), n = 20)
  expect_true(
    median(sapply(1:10, function(i) length(intersect(term1[,i], term2[,i])))) > 5,
  )

  # use cluster
  mat3 <- tail(as.matrix(dov_test, normalize = FALSE), 1000)
  rownames(km1$kmeans$centers) <- paste0("cluster", 1:10) # custom topic lables
  km3 <- textmodel_kmeans(mat3, centers = km1$kmeans$centers,
                          verbose = FALSE)
  expect_true(
    is.factor(topics(km3))
  )
  expect_equal(
    levels(topics(km3)),
    paste0("cluster", 1:10)
  )
  expect_equal(
    dim(predict(km3)),
    c(1000, 100)
  )

  term3 <- terms(km3, tail(dfmt_test, 1000), n = 20)
  expect_true(
    median(sapply(1:10, function(i) length(intersect(term1[,i], term3[,i])))) > 5
  )
})

