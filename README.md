
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
    ##     601    5866    7227    6926    7775   10906    5742    7632    7736    5293

``` r
terms(gmm, data = dfmt)
```

    ##       topic1        topic2         topic3       topic4     topic5      
    ##  [1,] "editing"     "government"   "militants"  "ap"       "said"      
    ##  [2,] "writing"     "presidential" "islamic"    "krasnaya" "ap"        
    ##  [3,] "stonestreet" "elections"    "syria"      "polyana"  "tsunami"   
    ##  [4,] "tait"        "president"    "iraq"       "sochi"    "passengers"
    ##  [5,] "chizu"       "minister"     "sunni"      "olympics" "ebola"     
    ##  [6,] "nomiyama"    "parliament"   "government" "new"      "tropical"  
    ##  [7,] "gutterman"   "prime"        "gaza"       "cannes"   "hurricane" 
    ##  [8,] "hepinstall"  "party"        "said"       "world"    "mh370"     
    ##  [9,] "pomeroy"     "election"     "military"   "olympic"  "people"    
    ## [10,] "bangalore"   "said"         "islamist"   "janeiro"  "south"     
    ##       topic6      topic7         topic8     topic9      topic10    
    ##  [1,] "said"      "champions"    "earnings" "said"      "police"   
    ##  [2,] "ukraine"   "championship" "futures"  "police"    "said"     
    ##  [3,] "kerry"     "rugby"        "index"    "murder"    "gunmen"   
    ##  [4,] "sanctions" "beats"        "billion"  "sentenced" "killed"   
    ##  [5,] "lavrov"    "1-0"          "said"     "pistorius" "killing"  
    ##  [6,] "russia"    "england"      "growth"   "court"     "militants"
    ##  [7,] "obama"     "ap"           "inc"      "ap"        "people"   
    ##  [8,] "u.s"       "scored"       "stock"    "rights"    "bomb"     
    ##  [9,] "crimea"    "2-0"          "percent"  "lawyer"    "soldiers" 
    ## [10,] "nuclear"   "3-0"          "tsx"      "sentence"  "ap"

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
    ##     8390    18691    10398     8410     8189    11626

``` r
terms(sgmm, data = dfmt)
```

    ##       economy    politics      security    sports      crimes       
    ##  [1,] "growth"   "lavrov"      "militants" "beats"     "pistorius"  
    ##  [2,] "stocks"   "kerry"       "islamic"   "6-4"       "sentence"   
    ##  [3,] "index"    "parliament"  "killing"   "innings"   "killing"    
    ##  [4,] "earnings" "nato"        "syria"     "6-3"       "crimes"     
    ##  [5,] "chrysler" "nuclear"     "militant"  "rugby"     "arrested"   
    ##  [6,] "tsx"      "poroshenko"  "army"      "6-2"       "murder"     
    ##  [7,] "futures"  "pro-russian" "shiite"    "3-0"       "brotherhood"
    ##  [8,] "wireless" "u.n"         "civilians" "twenty20"  "jury"       
    ##  [9,] "corp"     "erdogan"     "syrian"    "champions" "sentences"  
    ## [10,] "cents"    "resolution"  "sunni"     "2-0"       "girlfriend" 
    ##       other        
    ##  [1,] "editing"    
    ##  [2,] "hurricane"  
    ##  [3,] "stonestreet"
    ##  [4,] "magnitude"  
    ##  [5,] "rico"       
    ##  [6,] "earthquake" 
    ##  [7,] "tsunami"    
    ##  [8,] "rescuers"   
    ##  [9,] "killing"    
    ## [10,] "tropical"
