#install.packages("https://cran.r-project.org/src/contrib/Archive/GMTM/GMTM_0.1.0.tar.gz")

library(quanteda)
library(wordvector)
library(GMTM)

corp <- head(wordvector::data_corpus_news2014, 1000)
toks <- tokens(corp, remove_punct = TRUE,
               remove_symbols = TRUE, remove_numbers = TRUE) %>%
        tokens_remove(stopwords("en"), min_nchar = 2)
dfmt <- dfm(toks)
#saveRDS(dfmt, "tests/data/dfm.RDS")

wov <- textmodel_word2vec(toks, dim = 50)
dov <- as.textmodel_doc2vec(dfm(toks), wov)

gmm <- textmodel_gmm(dov, k = 10)

#saveRDS(gmm, paste0("tests/data/gmm_v", gmm$version, ".RDS"))

km <- textmodel_kmeans(dov, k = 10)

#saveRDS(km, "tests/data/kmeans_v", km$version, ".RDS")
