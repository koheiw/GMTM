
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
toks <- tokens(corp, remove_punct = TRUE, remove_symbols = TRUE, remove_number = TRUE) |> 
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
    ##    7376    5272   10970    3829    7954    9086    5649    9470     608    5649

``` r
terms(gmm, data = dfmt)
```

    ##       topic1     topic2       topic3    topic4        topic5     
    ##  [1,] "percent"  "president"  "u.s"     "ukraine"     "islamic"  
    ##  [2,] "earnings" "parliament" "said"    "russia"      "said"     
    ##  [3,] "inc"      "minister"   "kerry"   "annexation"  "militants"
    ##  [4,] "growth"   "polls"      "peace"   "putin"       "syria"    
    ##  [5,] "futures"  "party"      "ap"      "crimea"      "islamist" 
    ##  [6,] "said"     "election"   "united"  "russian"     "israeli"  
    ##  [7,] "index"    "prime"      "u.n"     "pro-russian" "qaeda"    
    ##  [8,] "corp"     "government" "talks"   "said"        "killed"   
    ##  [9,] "chrysler" "said"       "syria"   "separatists" "army"     
    ## [10,] "data"     "vote"       "islamic" "nato"        "sunni"    
    ##       topic6         topic7      topic8       topic9        topic10    
    ##  [1,] "beats"        "pistorius" "ap"         "editing"     "said"     
    ##  [2,] "championship" "sentenced" "said"       "writing"     "minister" 
    ##  [3,] "polyana"      "sentence"  "people"     "stonestreet" "told"     
    ##  [4,] "ap"           "court"     "police"     "tait"        "kerry"    
    ##  [5,] "2-0"          "ap"        "passengers" "chizu"       "news"     
    ##  [6,] "scored"       "trial"     "south"      "nomiyama"    "reuters"  
    ##  [7,] "rugby"        "prison"    "two"        "hepinstall"  "snowden"  
    ##  [8,] "1-0"          "guilty"    "city"       "pomeroy"     "statement"
    ##  [9,] "england"      "said"      "ebola"      "bangalore"   "foreign"  
    ## [10,] "6-4"          "sentences" "evacuated"  "grove"       "state"

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
    ##     9013    18622     9224    10160     8479    10365

``` r
terms(sgmm, data = dfmt)
```

    ##       economy    politics      security    sports     crimes       
    ##  [1,] "growth"   "lavrov"      "sunni"     "krasnaya" "sentence"   
    ##  [2,] "index"    "nato"        "militants" "polyana"  "pistorius"  
    ##  [3,] "earnings" "pro-russian" "islamic"   "beats"    "sentenced"  
    ##  [4,] "futures"  "poroshenko"  "syria"     "editing"  "killing"    
    ##  [5,] "chrysler" "resolution"  "killing"   "2-0"      "prison"     
    ##  [6,] "tsx"      "kerry"       "qaeda"     "6-4"      "brotherhood"
    ##  [7,] "retail"   "crimea"      "syrian"    "innings"  "girlfriend" 
    ##  [8,] "wireless" "yatseniuk"   "shiite"    "6-3"      "extradition"
    ##  [9,] "cents"    "separatists" "gunmen"    "rugby"    "terror"     
    ## [10,] "consumer" "steinmeier"  "islamists" "6-2"      "jail"       
    ##       other       
    ##  [1,] "tropical"  
    ##  [2,] "killing"   
    ##  [3,] "capsized"  
    ##  [4,] "tsunami"   
    ##  [5,] "rescuers"  
    ##  [6,] "volcano"   
    ##  [7,] "kilometres"
    ##  [8,] "wildfire"  
    ##  [9,] "landslides"
    ## [10,] "lava"
