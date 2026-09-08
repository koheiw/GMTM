
# GMTM: Gaussian Mixture Topic Models

An R package for unsupervised or semi-supervised topic analysis of dense
document vectors. **GMTM** performs clustering of document vectors using
Gaussian mixture models. Document vectors can be source from any package
but [wordvectors](https://github.com/koheiw/wordvector) works
seamlessly. The underlying function is based on the Armadillo library
for fast computation.

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
    ##    9151    9947    9637    6292    6366    4391    3823    6184    4957    5115

``` r
terms(gmm, data = dfmt)
```

    ##       topic1      topic2    topic3         topic4     topic5        topic6     
    ##  [1,] "ebola"     "kerry"   "editing"      "index"    "palestinian" "militants"
    ##  [2,] "leone"     "iaea"    "rugby"        "corp"     "syria"       "islamist" 
    ##  [3,] "tsunami"   "snowden" "beats"        "data"     "bashar"      "gunmen"   
    ##  [4,] "virus"     "nuclear" "championship" "earnings" "militants"   "qaeda"    
    ##  [5,] "mh370"     "obama"   "2-0"          "futures"  "islamist"    "levant"   
    ##  [6,] "quake"     "abe"     "1-0"          "co"       "qaeda"       "islamists"
    ##  [7,] "sierra"    "uranium" "england"      "cents"    "iraq"        "sanaa"    
    ##  [8,] "hurricane" "wto"     "6-4"          "wireless" "israeli"     "boko"     
    ##  [9,] "tropical"  "zarif"   "champions"    "stocks"   "islamic"     "shiite"   
    ## [10,] "antarctic" "atomic"  "coach"        "inc"      "israel"      "haram"    
    ##       topic7        topic8       topic9     topic10       
    ##  [1,] "annexation"  "pistorius"  "killing"  "polls"       
    ##  [2,] "pro-russian" "murder"     "kills"    "shinawatra"  
    ##  [3,] "poroshenko"  "sentenced"  "injuring" "bharatiya"   
    ##  [4,] "lavrov"      "sentence"   "bomber"   "janata"      
    ##  [5,] "crimean"     "prison"     "gunmen"   "dilma"       
    ##  [6,] "putin"       "guilty"     "rescuers" "vote"        
    ##  [7,] "sanctions"   "girlfriend" "hospital" "parliament"  
    ##  [8,] "crimea"      "jury"       "village"  "bjp"         
    ##  [9,] "separatists" "sentences"  "ferry"    "rousseff"    
    ## [10,] "nato"        "trial"      "wounding" "presidential"

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
    ##     8255    19168    10118     8463     7950    11909

``` r
terms(sgmm, data = dfmt)
```

    ##       economy    politics      security     sports      crimes       
    ##  [1,] "growth"   "lavrov"      "militants"  "rugby"     "sentence"   
    ##  [2,] "stocks"   "kerry"       "islamist"   "6-4"       "pistorius"  
    ##  [3,] "index"    "peace"       "sunni"      "innings"   "killing"    
    ##  [4,] "earnings" "parliament"  "islamic"    "6-3"       "sentenced"  
    ##  [5,] "futures"  "nato"        "killing"    "beats"     "arrested"   
    ##  [6,] "chrysler" "nuclear"     "iraq"       "6-2"       "crimes"     
    ##  [7,] "tsx"      "poroshenko"  "syria"      "3-0"       "prison"     
    ##  [8,] "wireless" "pro-russian" "insurgents" "twenty20"  "murder"     
    ##  [9,] "corp"     "u.n"         "army"       "champions" "brotherhood"
    ## [10,] "cents"    "resolution"  "qaeda"      "2-0"       "guantanamo" 
    ##       other        
    ##  [1,] "editing"    
    ##  [2,] "ebola"      
    ##  [3,] "hurricane"  
    ##  [4,] "stonestreet"
    ##  [5,] "magnitude"  
    ##  [6,] "earthquake" 
    ##  [7,] "tsunami"    
    ##  [8,] "ice"        
    ##  [9,] "rescuers"   
    ## [10,] "killing"
