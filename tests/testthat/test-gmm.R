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

test_that("textmodel_gmm works", {

  set.seed(1234)

  # use k
  mat1 <- head(as.matrix(dov_test, normalize = FALSE), 1000)
  gmm1 <- textmodel_gmm(mat1, k = 10, verbose = FALSE)

  expect_equal(
    names(gmm1),
    c("k", "centers", "likelihood", "cluster", "label", "docname")
  )
  expect_equal(
    names(topics(gmm1)),
    rownames(mat1),
  )
  expect_true(
    is.factor(topics(gmm1))
  )
  expect_equal(
    levels(topics(gmm1)),
    paste0("topic", 1:10)
  )
  # expect_equal(
  #   dim(predict(gmm1)),
  #   c(1000, 10)
  # )
  # expect_equal(
  #   dimnames(predict(gmm1)),
  #   list(rownames(mat1),
  #        paste0("topic", 1:10))
  # )
  # expect_equal(
  #   dim(predict(gmm1, newdata = mat1[1:5,])),
  #   c(5, 10)
  # )

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
  # expect_equal(
  #   dim(predict(gmm2)),
  #   c(1000, 10)
  # )

  term1 <- terms(gmm1, head(dfmt_test, 1000), n = 20)
  term2 <- terms(gmm2, tail(dfmt_test, 1000), n = 20)
  expect_true(
    all(sapply(1:10, function(i) length(intersect(term1[,i], term2[,i]))) > 1),
  )
})

