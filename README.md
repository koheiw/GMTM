
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
    ##    8153    6444    5710     625    9690    5497    8177    9028    8667    3872

``` r
terms(gmm, data = dfmt)
```

    ##       topic1       topic2     topic3       topic4        topic5       
    ##  [1,] "said"       "percent"  "president"  "editing"     "kerry"      
    ##  [2,] "u.s"        "futures"  "polls"      "writing"     "said"       
    ##  [3,] "ap"         "earnings" "minister"   "stonestreet" "islamic"    
    ##  [4,] "snowden"    "growth"   "parliament" "tait"        "peace"      
    ##  [5,] "minister"   "stocks"   "prime"      "chizu"       "syria"      
    ##  [6,] "co"         "index"    "party"      "nomiyama"    "nuclear"    
    ##  [7,] "news"       "inc"      "election"   "hepinstall"  "palestinian"
    ##  [8,] "department" "said"     "government" "pomeroy"     "united"     
    ##  [9,] "statement"  "corp"     "shinawatra" "bangalore"   "u.n"        
    ## [10,] "chief"      "cents"    "said"       "lowe"        "ap"         
    ##       topic6      topic7      topic8         topic9       topic10      
    ##  [1,] "pistorius" "islamist"  "beats"        "ap"         "ukraine"    
    ##  [2,] "sentenced" "islamic"   "championship" "said"       "russia"     
    ##  [3,] "court"     "said"      "krasnaya"     "tsunami"    "pro-russian"
    ##  [4,] "ap"        "sunni"     "polyana"      "passengers" "nato"       
    ##  [5,] "prison"    "militants" "ap"           "people"     "russian"    
    ##  [6,] "trial"     "gunmen"    "2-0"          "mh370"      "crimea"     
    ##  [7,] "sentence"  "boko"      "rugby"        "volcano"    "said"       
    ##  [8,] "said"      "killed"    "1-0"          "leone"      "kiev"       
    ##  [9,] "police"    "haram"     "6-4"          "ebola"      "ukraine's"  
    ## [10,] "killing"   "israeli"   "champions"    "mexico"     "annexation"
