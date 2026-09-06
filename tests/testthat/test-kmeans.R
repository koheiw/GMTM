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
km_test <- textmodel_kmeans(dov_test)

test_that("textmodel_kmeans works", {

  expect_equal(
    names(km_test),
    c("k", "centers", "cluster", "label", "docname", "call", "version")
  )
  expect_equal(
    names(topics(km_test)),
    rownames(dfmt_test),
  )
  expect_true(
    is.factor(topics(km_test))
  )
  expect_equal(
    levels(topics(km_test)),
    paste0("topic", 1:10)
  )
  expect_equal(
    dim(terms(km_test, dfmt_test, 15)),
    c(15, 10)
  )
  expect_output(
    print(km_test),
    "Call:\ntextmodel_kmeans\\(.*\\)"
  )
  expect_error(
    terms(km_test, head(dfmt_test, 100), n = 20),
    "the number of documents do not match"
  )

})

test_that("model works", {

  skip_on_cran()

  set.seed(1234)
  km1 <- textmodel_kmeans(dov_test, k = 15, verbose = FALSE)
  expect_message(
    km2 <- textmodel_kmeans(dov_test, model = km1, verbose = FALSE),
    "k is overwritten by the fitted model"
  )

  term1 <- terms(km1, dfmt_test, n = 10)
  term2 <- terms(km2, dfmt_test, n = 10)

  expect_true(
    all(sapply(1:15, function(i) length(intersect(term1[,i], term2[,i]))) > 0),
  )

})

