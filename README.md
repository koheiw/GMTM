
# GMTM: Gaussian Mixture Topic Models

## Installation

From CRAN:

``` r
#install.packages("GMTM") # not yet
```

From Github:

``` r
devtools::install_github("koheiw/GMTM")
```

``` r
library(quanteda)
library(wordvector)
library(GMTM)

# pre-processing (quanteda)
corp <- corpus_reshape(data_corpus_news2014)
toks <- tokens(corp, remove_punct = TRUE, remove_symbols = TRUE, remove_number = TRUE) |> 
  tokens_remove(stopwords("en"), min_nchar = 2)
dfmt <- dfm(toks)

# create document vector (wordvector)
wov <- textmodel_word2vec(toks, dim = 100)
dov <- as.textmodel_doc2vec(dfmt, wov)

# train Gaussian mixture model (GMTM)
gmm <- textmodel_gmm(dov, k = 10)
table(topics(gmm))
```

    ## 
    ##  topic1  topic2  topic3  topic4  topic5  topic6  topic7  topic8  topic9 topic10 
    ##     606    7000    5804    8445    6561    6138    7910    4994    6301   12104

``` r
terms(gmm, data = dfmt)
```

    ##       topic1        topic2      topic3       topic4      topic5      
    ##  [1,] "editing"     "militants" "president"  "rugby"     "pistorius" 
    ##  [2,] "writing"     "islamic"   "minister"   "beats"     "said"      
    ##  [3,] "stonestreet" "iraq"      "parliament" "champions" "court"     
    ##  [4,] "tait"        "syria"     "prime"      "ap"        "ap"        
    ##  [5,] "chizu"       "said"      "polls"      "2-0"       "trial"     
    ##  [6,] "nomiyama"    "gaza"      "party"      "scored"    "sentenced" 
    ##  [7,] "gutterman"   "militant"  "election"   "england"   "girlfriend"
    ##  [8,] "hepinstall"  "levant"    "government" "6-4"       "jury"      
    ##  [9,] "pomeroy"     "islamist"  "said"       "innings"   "killing"   
    ## [10,] "bangalore"   "sunni"     "tayyip"     "coach"     "police"    
    ##       topic6       topic7       topic8      topic9     topic10  
    ##  [1,] "ap"         "kerry"      "said"      "index"    "said"   
    ##  [2,] "said"       "sanctions"  "gunmen"    "growth"   "ap"     
    ##  [3,] "ferry"      "lavrov"     "killed"    "earnings" "u.s"    
    ##  [4,] "passengers" "ukraine"    "police"    "billion"  "leone"  
    ##  [5,] "tsunami"    "said"       "militants" "corp"     "reuters"
    ##  [6,] "mh370"      "russia"     "killing"   "data"     "says"   
    ##  [7,] "puerto"     "peace"      "people"    "futures"  "china"  
    ##  [8,] "debris"     "president"  "ap"        "percent"  "sierra" 
    ##  [9,] "quake"      "annexation" "bomb"      "inc"      "us"     
    ## [10,] "rescuers"   "poroshenko" "wounding"  "said"     "snowden"
