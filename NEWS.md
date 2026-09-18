## GMTM version 0.2.0

- Add `probability()` to extract topic likelihood of documents.
- Add `seeds` to `textmodel_kmeans()` and `textmodel_gmm()` to initialize clustering with user-provided centers.
- Add `as.seedwords()` to create a seed words matrix from **quanteda** dictionaries.

## GMTM version 0.1.0

- Create `textmodel_kmeans()` and `textmodel_gmm()` as wrappers around Armadillo's `kmeans` and `gmm_diag` functions, respectively.
- Create `terms()` and `topics()` as utility functions for topic analysis. 
