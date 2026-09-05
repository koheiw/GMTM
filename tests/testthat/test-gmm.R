library(quanteda)
library(wordvector)
library(GMTM)

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
    c("k", "centers", "likelihood", "cluster", "label", "docname", "call", "version")
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
  gmm1 <- textmodel_gmm(dov_test, k = 10, verbose = FALSE)
  gmm2 <- textmodel_gmm(dov_test, model = gmm1, verbose = FALSE)

  term1 <- terms(gmm1, dfmt_test, n = 10)
  term2 <- terms(gmm2, dfmt_test, n = 10)

  expect_true(
    all(sapply(1:10, function(i) length(intersect(term1[,i], term2[,i]))) > 0),
  )

})

