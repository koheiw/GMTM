
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
    ##    5723   10008    8335    6383    5885    5186     801    8978    7020    7544

``` r
terms(gmm, data = dfmt)
```

    ##       topic1       topic2       topic3         topic4      topic5     
    ##  [1,] "president"  "ebola"      "rugby"        "percent"   "killed"   
    ##  [2,] "minister"   "passengers" "champions"    "index"     "police"   
    ##  [3,] "parliament" "leone"      "beats"        "billion"   "killing"  
    ##  [4,] "prime"      "south"      "championship" "growth"    "people"   
    ##  [5,] "polls"      "tsunami"    "6-4"          "earnings"  "gunmen"   
    ##  [6,] "party"      "quake"      "scored"       "futures"   "militants"
    ##  [7,] "election"   "earthquake" "coach"        "stocks"    "kills"    
    ##  [8,] "government" "ferry"      "innings"      "inc"       "bomber"   
    ##  [9,] "shinawatra" "hurricane"  "6-3"          "quarterly" "mortar"   
    ## [10,] "bharatiya"  "mh370"      "2-0"          "cents"     "bomb"     
    ##       topic6       topic7        topic8       topic9      topic10     
    ##  [1,] "pistorius"  "editing"     "snowden"    "militants" "russia"    
    ##  [2,] "court"      "writing"     "minister"   "islamic"   "sanctions" 
    ##  [3,] "murder"     "stonestreet" "co"         "syria"     "lavrov"    
    ##  [4,] "sentenced"  "tait"        "government" "levant"    "merkel"    
    ##  [5,] "sentences"  "chizu"       "u.n"        "sunni"     "putin"     
    ##  [6,] "girlfriend" "nomiyama"    "us"         "syrian"    "nato"      
    ##  [7,] "killing"    "bangalore"   "statement"  "bashar"    "annexation"
    ##  [8,] "prison"     "dalgleish"   "news"       "qaeda"     "kerry"     
    ##  [9,] "woman"      "maler"       "department" "militant"  "president" 
    ## [10,] "jail"       "hepinstall"  "united"     "iraq"      "nuclear"

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
    ##     8800     6828     9870     9857    16493    14015

``` r
terms(sgmm, data = dfmt)
```

    ##       economy    politics     security      sports      crimes      
    ##  [1,] "index"    "parliament" "militants"   "editing"   "pistorius" 
    ##  [2,] "data"     "bharatiya"  "islamic"     "rugby"     "killing"   
    ##  [3,] "earnings" "janata"     "killing"     "krasnaya"  "rico"      
    ##  [4,] "futures"  "erdogan"    "syria"       "polyana"   "passengers"
    ##  [5,] "chrysler" "bjp"        "army"        "beats"     "sentenced" 
    ##  [6,] "profit"   "shinawatra" "observatory" "2-0"       "girlfriend"
    ##  [7,] "tsx"      "reform"     "civilians"   "6-4"       "sierra"    
    ##  [8,] "wireless" "speaker"    "syrian"      "liverpool" "mh370"     
    ##  [9,] "stocks"   "tayyip"     "sunni"       "1-0"       "crimes"    
    ## [10,] "cents"    "abdullah"   "bombing"     "striker"   "leone"     
    ##       other        
    ##  [1,] "kerry"      
    ##  [2,] "lavrov"     
    ##  [3,] "nuclear"    
    ##  [4,] "u.n"        
    ##  [5,] "syria"      
    ##  [6,] "crimea"     
    ##  [7,] "separatists"
    ##  [8,] "islamic"    
    ##  [9,] "pro-russian"
    ## [10,] "palestinian"
