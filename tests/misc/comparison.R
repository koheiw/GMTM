library(quanteda)
library(wordvector)

corp <- readRDS(file.path("D:/Research/Torch-test/data", "corpus_ungd.RDS"))
# corp <- seededlda::data_corpus_moviereviews |>
#   corpus_segment("[.?!]", valuetype = "regex")

corp <- corpus_reshape(wordmap::data_corpus_ungd2017)
toks <- tokens(corp, remove_punct = TRUE, remove_symbols = TRUE, remove_number = TRUE) |>
  tokens_remove(stopwords("en"), min_nchar = 2) |>
  tokens_subset(min_ntoken = 2)

wov <- textmodel_word2vec(toks, dim = 100, min_count = 5)
head(similarity(wov, "war"), 10)

dfmt <- dfm(toks, remove_padding = TRUE) |>
  dfm_subset(year >= 1991)
#dfmt <- head(dfmt, 10000)
dov <- as.textmodel_doc2vec(dfmt, wov)
#dov <- textmodel_doc2vec(toks, dim = 100, type = "dm", window = 100)

mat <- as.matrix(dov, normalize = FALSE)
mat[is.nan(mat)] <- 0


#-------------------------------

set.seed(1234)
km <- kmeans(mat, 20, algorithm = "Lloyd", iter.max = 100)
GMTM:::get_terms(km$cluster, dfmt)
table(km$cluster)
km$center

library(ClusterR)

#Sys.setenv(OMP_NUM_THREADS = 4)
gmm <- GMM(mat, 100, "eucl_dist", "random_subset", 10, km_iter = 10, em_iter = 0,
           #var_floor = 1e-20,
           verbose = TRUE)

cluster <- max.col(gmm$Log_likelihood)
table(cluster)
GMTM:::get_terms(factor(cluster), dfmt, min_count = 1)

pred <- predict(gmm, mat)

pred <- predict_GMM(mat, gmm$centroids, gmm$covariance_matrices, gmm$weights)
table(pred$cluster_labels)

library(flexmix)

flx <- flexmix(x ~ 1, data = list(x = mat), k = 10,
               model = FLXMCmvnorm(diagonal = TRUE),
               control = list(verbose = 1,
                              minprior = 0))
GMTM:::get_terms(flx@cluster, dfmt, min_count = 1)

library(seededlda)
lda <- textmodel_lda(dfmt, 10, batch_size = 0.01, auto_iter = TRUE)
terms(lda)

library(dbscan)
snn <- sNNclust(mat, k = 20, eps = 0.1, minPts = 10)
table(snn$cluster)
get_terms(dfmt, snn$cluster)

#opt <- optics(mat)
#opt$coredist

dbs <- dbscan(mat2, minPts = 10, eps = 1)
table(dbs$cluster)
get_terms(dfmt, dbs$cluster)

library(mclust)
mc <- Mclust(mat2, G = 20, modelNames = c("VII", "EII"))
summary(mc)

GMTM::get_terms(predict(mc)$classification, dfmt)

library(umap)
conf <- umap.defaults
conf$n_neighbors = 15
conf$n_components = 10

ump <- umap(mat, conf)
plot(ump$layout[,c(3, 6)])
dbs <- dbscan(ump$layout, eps = 0.6, minPts = 2)
