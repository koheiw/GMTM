
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

## Example

**GMTM** is applied to identify topics of sentences from 20,000 news
articles. Before clustering,
[quanteda](https://github.com/quanteda/quanteda) and
[wordvectors](https://github.com/koheiw/wordvector) are used for
pre-processing of textual data and training document vectors,
respectively.

``` r
library(quanteda)
library(wordvector)
library(GMTM)

# pre-processing (quanteda)
corp <- corpus_reshape(data_corpus_news2014)
toks <- tokens(corp, remove_punct = TRUE, remove_symbols = TRUE, remove_number = TRUE) |> 
  tokens_remove(stopwords("en"), min_nchar = 2)
dfmt <- dfm(toks)

# train document vectors (wordvector)
wov <- textmodel_word2vec(toks, dim = 100)
dov <- as.textmodel_doc2vec(dfmt, wov)

# fit Gaussian mixture model (GMTM)
gmm <- textmodel_gmm(dov, k = 10)
table(topics(gmm))
```

    ## 
    ##  topic1  topic2  topic3  topic4  topic5  topic6  topic7  topic8  topic9 topic10 
    ##    7002    5233    8618    6602    8463    8896    5913    9198     605    5333

``` r
terms(gmm, data = dfmt)
```

    ##       topic1      topic2      topic3       topic4      topic5      
    ##  [1,] "militants" "index"     "ap"         "ap"        "ukraine"   
    ##  [2,] "islamic"   "earnings"  "said"       "court"     "said"      
    ##  [3,] "islamist"  "billion"   "ebola"      "trial"     "russia"    
    ##  [4,] "syria"     "futures"   "mh370"      "murder"    "lavrov"    
    ##  [5,] "sunni"     "growth"    "south"      "sentenced" "sanctions" 
    ##  [6,] "said"      "stocks"    "tsunami"    "pistorius" "kerry"     
    ##  [7,] "qaeda"     "percent"   "ferry"      "said"      "nato"      
    ##  [8,] "levant"    "economy"   "hurricane"  "jail"      "putin"     
    ##  [9,] "syrian"    "quarterly" "passengers" "killing"   "president" 
    ## [10,] "state"     "profit"    "tropical"   "sentence"  "annexation"
    ##       topic6         topic7         topic8      topic9        topic10    
    ##  [1,] "rugby"        "presidential" "said"      "editing"     "said"     
    ##  [2,] "champions"    "president"    "u.s"       "writing"     "killed"   
    ##  [3,] "beats"        "minister"     "reuters"   "stonestreet" "police"   
    ##  [4,] "championship" "parliament"   "co"        "tait"        "killing"  
    ##  [5,] "polyana"      "prime"        "ap"        "chizu"       "people"   
    ##  [6,] "ap"           "polls"        "snowden"   "nomiyama"    "attack"   
    ##  [7,] "2-0"          "party"        "told"      "hepinstall"  "gunmen"   
    ##  [8,] "6-4"          "election"     "corp"      "pomeroy"     "wounded"  
    ##  [9,] "krasnaya"     "government"   "executive" "bangalore"   "militants"
    ## [10,] "coach"        "shinawatra"   "inc"       "heinrich"    "ap"
