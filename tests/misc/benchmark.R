library(quanteda)
library(wordvector)
library(seededlda)
library(GMTM)

corp <- corpus_reshape(data_corpus_news2014)
toks <- tokens(corp, remove_punct = TRUE, remove_symbols = TRUE, remove_number = TRUE) |>
  tokens_remove(stopwords("en"), min_nchar = 2) |>
  tokens_tolower() |>
  tokens_trim(min_termfreq = 5)
dfmt <- dfm(toks)

system.time({
  wov <- textmodel_word2vec(toks, dim = 100)
  dov <- as.textmodel_doc2vec(dfmt, wov)
  gmm <- textmodel_gmm(dov, k = 10)
})
GMTM::terms(gmm, dfmt)

system.time({
  lda <- textmodel_lda(dfmt, k = 10, batch_size = 0.01, auto_iter = TRUE)
})
seededlda::terms(lda)
