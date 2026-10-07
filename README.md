
# GMTM: Gaussian Mixture Topic Models

An R package for unsupervised or semi-supervised topic analysis of dense
document vectors. **GMTM** performs clustering of document vectors using
Gaussian mixture models. Document vectors can be source from any package
but [wordvectors](https://github.com/koheiw/wordvector) works
seamlessly. The underlying function is based on the [Armadillo
library](https://arma.sourceforge.net/docs.html#gmm_diag) for fast
computation. The algorithm is explain in the article below:

Sanderson, C., & Curtin, R. (2017). *An open source C++ implementation
of multi-threaded Gaussian mixture models, k-means and expectation
maximisation*. <https://doi.org/10.1109/ICSPCS.2017.8270510>

## Installation

From CRAN:

``` r
#install.packages("GMTM") # not yet
```

From Github:

``` r
devtools::install_github("koheiw/GMTM")
```

## Examples

**GMTM** is applied to identify topics of sentences from 20,000 news
articles. Before clustering,
[quanteda](https://github.com/quanteda/quanteda) and
[wordvectors](https://github.com/koheiw/wordvector) are used for
pre-processing textual data and training document vectors, respectively.

``` r
library(quanteda)
library(wordvector)
library(GMTM)

# pre-processing using quanteda
corp <- corpus_reshape(data_corpus_news2014)
toks <- tokens(corp, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE) |> 
  tokens_remove(stopwords("en"), min_nchar = 2)
dfmt <- dfm(toks)

# train document vectors using wordvector
wov <- textmodel_word2vec(toks, dim = 100)
dov <- as.textmodel_doc2vec(dfmt, wov)
```

### Unsupervised analysis

The basic usage of the package is discovering user-defined number of
topics, `k`.

``` r
gmm <- textmodel_gmm(dov, k = 10)
table(topics(gmm))
```

    ## 
    ##  topic1  topic2  topic3  topic4  topic5  topic6  topic7  topic8  topic9 topic10 
    ##     600    6013    7129    8387   10264    5165    8415    6970    6941    5820

``` r
terms(gmm, data = dfmt)
```

    ##       topic1        topic2       topic3      topic4      topic5    topic6   
    ##  [1,] "reporting"   "president"  "said"      "ukraine"   "ap"      "court"  
    ##  [2,] "editing"     "minister"   "state"     "russia"    "said"    "ap"     
    ##  [3,] "writing"     "prime"      "islamic"   "said"      "new"     "said"   
    ##  [4,] "john"        "party"      "syria"     "u.s"       "south"   "trial"  
    ##  [5,] "bangalore"   "election"   "iraq"      "president" "people"  "police" 
    ##  [6,] "heritage"    "government" "group"     "russian"   "says"    "former" 
    ##  [7,] "timothy"     "said"       "militants" "ap"        "china"   "death"  
    ##  [8,] "steve"       "new"        "forces"    "united"    "reuters" "man"    
    ##  [9,] "michael"     "reuters"    "ap"        "talks"     "ebola"   "charges"
    ## [10,] "stonestreet" "vote"       "gaza"      "sanctions" "two"     "case"   
    ##       topic7      topic8    topic9       topic10    
    ##  [1,] "ap"        "percent" "said"       "said"     
    ##  [2,] "world"     "said"    "told"       "police"   
    ##  [3,] "cup"       "reuters" "minister"   "killed"   
    ##  [4,] "first"     "billion" "reuters"    "people"   
    ##  [5,] "new"       "u.s"     "u.s"        "ap"       
    ##  [6,] "win"       "million" "statement"  "two"      
    ##  [7,] "league"    "year"    "news"       "least"    
    ##  [8,] "australia" "new"     "state"      "city"     
    ##  [9,] "england"   "bank"    "government" "attack"   
    ## [10,] "team"      "ap"      "foreign"    "officials"

### Semi-supervised analysis

It is also possible to use seed words to define topics: use
`as.seedwords()` to create a seed matrix and pass it to `seeds`.

``` r
dict <- dictionary(list(economy = "econom*", politics = "politi*", 
                        security = c("securit*", "milita*"), 
                        sports = "sport*", crimes = "crime"))

seed <- as.seedwords(dict, wov, residual = 1)
sgmm <- textmodel_gmm(dov, seeds = seed)
```

    ## k is overwritten by the seeds

``` r
table(topics(sgmm))
```

    ## 
    ##  economy politics security   sports   crimes    other 
    ##     9032    18019     9954     8683     8126    11890

``` r
terms(sgmm, data = dfmt)
```

    ##       economy   politics     security    sports      crimes   other      
    ##  [1,] "said"    "said"       "said"      "ap"        "said"   "ap"       
    ##  [2,] "percent" "president"  "killed"    "world"     "ap"     "said"     
    ##  [3,] "reuters" "ukraine"    "state"     "cup"       "court"  "people"   
    ##  [4,] "u.s"     "minister"   "ap"        "new"       "police" "south"    
    ##  [5,] "billion" "russia"     "people"    "first"     "former" "new"      
    ##  [6,] "million" "ap"         "police"    "win"       "trial"  "reuters"  
    ##  [7,] "new"     "government" "militants" "australia" "death"  "reporting"
    ##  [8,] "year"    "reuters"    "islamic"   "league"    "man"    "city"     
    ##  [9,] "bank"    "new"        "forces"    "england"   "years"  "two"      
    ## [10,] "ap"      "united"     "iraq"      "team"      "new"    "editing"
