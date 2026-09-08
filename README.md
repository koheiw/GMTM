
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
    ##    6891    8318    1119    5982    5479    6628    6905    8414   10366    5761

``` r
terms(gmm, data = dfmt)
```

    ##       topic1        topic2      topic3        topic4       topic5      
    ##  [1,] "russia"      "militants" "editing"     "minister"   "pistorius" 
    ##  [2,] "sanctions"   "islamic"   "writing"     "statement"  "murder"    
    ##  [3,] "lavrov"      "syria"     "stonestreet" "news"       "sentenced" 
    ##  [4,] "kerry"       "militant"  "tait"        "department" "court"     
    ##  [5,] "annexation"  "killed"    "chizu"       "kerry"      "prison"    
    ##  [6,] "pro-russian" "gaza"      "nomiyama"    "government" "judge"     
    ##  [7,] "president"   "army"      "dalgleish"   "chief"      "sentence"  
    ##  [8,] "peace"       "syrian"    "maler"       "official"   "police"    
    ##  [9,] "russian"     "sunni"     "hepinstall"  "foreign"    "killing"   
    ## [10,] "crimea"      "qaeda"     "bangalore"   "snowden"    "girlfriend"
    ##       topic6       topic7     topic8         topic9    topic10     
    ##  [1,] "ferry"      "percent"  "rugby"        "ebola"   "president" 
    ##  [2,] "people"     "index"    "beats"        "leone"   "vote"      
    ##  [3,] "quake"      "futures"  "championship" "china"   "minister"  
    ##  [4,] "passengers" "growth"   "scored"       "un"      "parliament"
    ##  [5,] "police"     "earnings" "6-4"          "south"   "shinawatra"
    ##  [6,] "rescuers"   "stocks"   "champions"    "united"  "prime"     
    ##  [7,] "typhoon"    "corp"     "innings"      "sierra"  "polls"     
    ##  [8,] "capsized"   "profit"   "6-3"          "us"      "party"     
    ##  [9,] "tropical"   "cents"    "2-0"          "climate" "election"  
    ## [10,] "city"       "data"     "1-0"          "liberia" "elections"

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
    ##     8374    18310    10292     8436     8416    12035

``` r
terms(sgmm, data = dfmt)
```

    ##       economy    politics      security    sports     crimes       
    ##  [1,] "growth"   "lavrov"      "militants" "beats"    "pistorius"  
    ##  [2,] "index"    "kerry"       "islamic"   "6-4"      "sentence"   
    ##  [3,] "earnings" "parliament"  "killing"   "innings"  "killing"    
    ##  [4,] "futures"  "nuclear"     "syria"     "6-3"      "prison"     
    ##  [5,] "inc"      "poroshenko"  "militant"  "scored"   "jury"       
    ##  [6,] "chrysler" "pro-russian" "syrian"    "rugby"    "brotherhood"
    ##  [7,] "tsx"      "u.n"         "shiite"    "6-2"      "girlfriend" 
    ##  [8,] "wireless" "resolution"  "mortar"    "3-0"      "terror"     
    ##  [9,] "stocks"   "crimea"      "civilians" "twenty20" "sentenced"  
    ## [10,] "corp"     "yatseniuk"   "rebels"    "2-0"      "jail"       
    ##       other        
    ##  [1,] "editing"    
    ##  [2,] "ebola"      
    ##  [3,] "tsunami"    
    ##  [4,] "hurricane"  
    ##  [5,] "stonestreet"
    ##  [6,] "ice"        
    ##  [7,] "earthquake" 
    ##  [8,] "tropical"   
    ##  [9,] "rescuers"   
    ## [10,] "writing"
