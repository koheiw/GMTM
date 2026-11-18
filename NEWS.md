## GMTM version 0.3.0

- Add `include_prob` to `textmodel_gmm()` to reduce the size of `textmodel_gmm` when it is `FALSE`.
- Change the structure of objects while providing compatibility with the earlier versions of the package.
- Change the `omit` argument to experimental.

## GMTM version 0.2.1

- Improve the consistency of the `textmodel_kmeans` and `textmodel_gmm` objects.
- Change the method to identify the most likely topic terms for `textmodel_gmm`.
- Fix the handling of multi-word expressions in `as.seedwords()`.

## GMTM version 0.2.0

- Add `probability()` to extract topic likelihood of documents.
- Add `seeds` to `textmodel_kmeans()` and `textmodel_gmm()` to initialize clustering with user-provided centers.
- Add `as.seedwords()` to create a seed words matrix from **quanteda** dictionaries.

## GMTM version 0.1.0

- Create `textmodel_kmeans()` and `textmodel_gmm()` as wrappers around Armadillo's `kmeans` and `gmm_diag` functions, respectively.
- Create `terms()` and `topics()` as utility functions for topic analysis. 
