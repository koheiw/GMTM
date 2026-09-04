library(quanteda)
library(wordvector)

corp <- readRDS(file.path("D:/Research/Torch-test/data", "corpus_ungd.RDS"))
# corp <- seededlda::data_corpus_moviereviews |>
#   corpus_segment("[.?!]", valuetype = "regex")

toks <- tokens(corp, remove_punct = TRUE, remove_symbols = TRUE, remove_number = TRUE) |>
  tokens_remove(stopwords("en"), min_nchar = 2) |>
  tokens_subset(min_ntoken = 2)

wov <- textmodel_word2vec(toks, dim = 100, min_count = 5)
head(similarity(wov, "war"), 10)

dfmt <- dfm(toks, remove_padding = TRUE)
dfmt <- head(dfmt, 1000)
dov <- as.textmodel_doc2vec(dfmt, wov)
#dov <- textmodel_doc2vec(toks, dim = 100, type = "dm", window = 100)

mat <- as.matrix(dov, normalize = TRUE)
mat[is.nan(mat)] <- 0

#-------------------------------

set.seed(1234)
km <- kmeans(mat, 20, algorithm = "Lloyd", iter.max = 100)
EBTM:::get_terms(km$cluster, dfmt)
table(km$cluster)
km$center

#lapply(tokens_sample(tokens_subset(toks, cluster == 3), 10), paste0, collapse = " ")

# library(amap)
# km2 <- Kmeans(mat, 10, iter.max = 100)
# get_terms(dfmt, km2$cluster)

library(dbscan)
snn <- sNNclust(mat, k = 20, eps = 0.1, minPts = 10)
table(snn$cluster)
get_terms(dfmt, snn$cluster)

#opt <- optics(mat)
#opt$coredist

dbs <- dbscan(mat2, minPts = 10, eps = 1)
table(dbs$cluster)
get_terms(dfmt, dbs$cluster)

# try embeddings from CNN
library(mclust)
mc <- Mclust(mat2, G = 20, modelNames = c("VII", "EII"))
summary(mc)
defaultPrior(mat, G = 10, modelName = c("VII"))
defaultPrior(mat, G = 10, modelName = c("EII"))

get_terms(dfmt, predict(mc)$classification)


library(umap)
conf <- umap.defaults
conf$n_neighbors = 15
conf$n_components = 10

ump <- umap(mat, conf)
plot(ump$layout[,c(3, 6)])

dbs <- dbscan(ump$layout, eps = 0.6, minPts = 2)
dbs
#dbs <- dbscan(frNN(mat, eps = 10), minPts = 2)
plot(mat, col = dbs$cluster)
points(mat[dbs$cluster == 1, ], pch = 3, col = "grey")

library(seededlda)
lda <- textmodel_lda(dfmt, 10, batch_size = 0.01, auto_iter = TRUE)
terms(lda)

#-------------------------

embed <- readRDS("D:/Research/Torch-test/result/matrix_ff_ungd.RDS")
embed <- readRDS("D:/Research/Torch-test/result/matrix_cnn_ungd.RDS")
embed <- readRDS("D:/Research/Torch-test/result/matrix_gru_ungd.RDS")
wov2 <- as.textmodel_word2vec(embed[-1,])
head(similarity(wov, "america"), 10)
head(similarity(wov2, "america"), 10)
head(similarity(wov2, "war"), 10)
head(similarity(wov2, "rights"), 10)
dov2 <- as.textmodel_doc2vec(dfmt, wov2)

mat2 <- as.matrix(dov2)
km2 <- kmeans(mat2, 20, algorithm = "Lloyd", iter.max = 100)
get_terms(dfmt, km2$cluster)
table(km2$cluster)

