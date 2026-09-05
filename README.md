
# GMTM: Gaussian Mixture Text Models for Topic Analysis

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
    ##    6609    6663    1297   10494    6980    5358    6001    8449    5991    8021

``` r
terms(gmm, data = dfmt)
```

    ##       topic1       topic2     topic3        topic4       topic5     
    ##  [1,] "minister"   "percent"  "editing"     "ebola"      "militants"
    ##  [2,] "snowden"    "index"    "writing"     "malaysia"   "islamic"  
    ##  [3,] "statement"  "billion"  "stonestreet" "virus"      "syria"    
    ##  [4,] "government" "growth"   "tait"        "south"      "iraq"     
    ##  [5,] "chief"      "earnings" "chizu"       "ferry"      "levant"   
    ##  [6,] "jen"        "futures"  "nomiyama"    "quake"      "sunni"    
    ##  [7,] "news"       "stocks"   "bangalore"   "hurricane"  "qaeda"    
    ##  [8,] "foreign"    "inc"      "dalgleish"   "outbreak"   "syrian"   
    ##  [9,] "official"   "prices"   "maler"       "passengers" "bashar"   
    ## [10,] "agency"     "cents"    "hepinstall"  "tropical"   "militant" 
    ##       topic6      topic7      topic8         topic9       topic10     
    ##  [1,] "pistorius" "killed"    "rugby"        "president"  "russia"    
    ##  [2,] "court"     "police"    "beats"        "minister"   "sanctions" 
    ##  [3,] "murder"    "killing"   "championship" "vote"       "president" 
    ##  [4,] "sentenced" "people"    "2-0"          "parliament" "annexation"
    ##  [5,] "sentence"  "militants" "6-4"          "prime"      "kerry"     
    ##  [6,] "jail"      "kills"     "champions"    "polls"      "nuclear"   
    ##  [7,] "prison"    "bomber"    "scored"       "party"      "russian"   
    ##  [8,] "woman"     "gunmen"    "coach"        "election"   "crimea"    
    ##  [9,] "killing"   "mortar"    "innings"      "government" "nato"      
    ## [10,] "police"    "bomb"      "6-3"          "shinawatra" "lavrov"
