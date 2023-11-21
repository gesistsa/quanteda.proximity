
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quanteda.proximity

<!-- badges: start -->

[![R-CMD-check](https://github.com/gesistsa/quanteda.proximity/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gesistsa/quanteda.proximity/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of quanteda.proximity is to add proximity vectors into the
`tokens` object of `quanteda`.

Proximity is measured by the number of tokens away from the keyword.
Given a tokenized sentence: \[“I”, “wash”, “this”, “apple”\] and suppose
“eat” is the keyword. The proximity vector is a vector with the same
length as the tokenized sentence and the values (using the default
settings) are \[2, 1, 2, 3\].

## Installation

You can install the development version of quanteda.proximity like so:

``` r
remotes::install_github("gesistsa/quanteda.proximity")
```

## Example

``` r
suppressPackageStartupMessages(library(quanteda))
library(quanteda.proximity)

txt1 <-
c("Turkish President Tayyip Erdogan, in his strongest comments yet on the Gaza conflict, said on Wednesday the Palestinian militant group Hamas was not a terrorist organisation but a liberation group fighting to protect Palestinian lands.",
"EU policymakers proposed the new agency in 2021 to stop financial firms from aiding criminals and terrorists. Brussels has so far relied on national regulators with no EU authority to stop money laundering and terrorist financing running into billions of euros.")
```

`tokens_proximity()` generates the proximity vectors and stores them as
a `docvar` (document variable).

``` r
tok1 <- txt1 %>% tokens() %>%
    tokens_proximity(pattern = "turkish")
tok1
#> Tokens consisting of 2 documents.
#> text1 :
#>  [1] "turkish"   "president" "tayyip"    "erdogan"   ","         "in"       
#>  [7] "his"       "strongest" "comments"  "yet"       "on"        "the"      
#> [ ... and 26 more ]
#> 
#> text2 :
#>  [1] "eu"           "policymakers" "proposed"     "the"          "new"         
#>  [6] "agency"       "in"           "2021"         "to"           "stop"        
#> [11] "financial"    "firms"       
#> [ ... and 31 more ]
#> 
#> With proximity vector(s).
#> Pattern:  turkish
```

You can access the proximity vectors by

``` r
docvars(tok1, "proximity")
#> $text1
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#> [26] 26 27 28 29 30 31 32 33 34 35 36 37 38
#> 
#> $text2
#>  [1] 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44
#> [26] 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44
```

The `tokens` object with proximity vectors can be converted to a
(weighted) `dfm` (Document-Feature Matrix). The default weight is
assigned by inverting the proximity.

``` r
dfm(tok1)
#> Document-feature matrix of: 2 documents, 64 features (45.31% sparse) and 0 docvars.
#>        features
#> docs    turkish president    tayyip erdogan         ,         in       his
#>   text1       1       0.5 0.3333333    0.25 0.2666667 0.16666667 0.1428571
#>   text2       0       0   0            0    0         0.02272727 0        
#>        features
#> docs    strongest  comments yet
#>   text1     0.125 0.1111111 0.1
#>   text2     0     0         0  
#> [ reached max_nfeat ... 54 more features ]
```

You have the freedom to change to another weight function. For example,
not inverting.

``` r
dfm(tok1, weight_function = identity)
#> Document-feature matrix of: 2 documents, 64 features (45.31% sparse) and 0 docvars.
#>        features
#> docs    turkish president tayyip erdogan  , in his strongest comments yet
#>   text1       1         2      3       4 20  6   7         8        9  10
#>   text2       0         0      0       0  0 44   0         0        0   0
#> [ reached max_nfeat ... 54 more features ]
```

Or any custom function

``` r
dfm(tok1, weight_function = function(x) { 1 / x^2 })
#> Document-feature matrix of: 2 documents, 64 features (45.31% sparse) and 0 docvars.
#>        features
#> docs    turkish president    tayyip erdogan          ,           in        his
#>   text1       1      0.25 0.1111111  0.0625 0.04444444 0.0277777778 0.02040816
#>   text2       0      0    0          0      0          0.0005165289 0         
#>        features
#> docs    strongest   comments  yet
#>   text1  0.015625 0.01234568 0.01
#>   text2  0        0          0   
#> [ reached max_nfeat ... 54 more features ]
```

## Application

A clumsy example to calculate the total inverse proximity weighted
frequency of "terror\*" words.

``` r
dict1 <- dictionary(list(TERROR = c("terror*")))

dfm(tok1) %>% dfm_lookup(dict1) %>% rowSums()
#>      text1      text2 
#> 0.03703704 0.04545455
```

How about changing the target to “Hamas”?

``` r
tok2 <- tok1 %>% tokens_proximity(pattern = "hamas")
tok2
#> Tokens consisting of 2 documents.
#> text1 :
#>  [1] "turkish"   "president" "tayyip"    "erdogan"   ","         "in"       
#>  [7] "his"       "strongest" "comments"  "yet"       "on"        "the"      
#> [ ... and 26 more ]
#> 
#> text2 :
#>  [1] "eu"           "policymakers" "proposed"     "the"          "new"         
#>  [6] "agency"       "in"           "2021"         "to"           "stop"        
#> [11] "financial"    "firms"       
#> [ ... and 31 more ]
#> 
#> With proximity vector(s).
#> Pattern:  hamas
```

``` r
dfm(tok2) %>% dfm_lookup(dict1) %>% rowSums()
#>      text1      text2 
#> 0.20000000 0.04545455
```

Can we use two targets, e.g. “EU” and “Brussels”?

``` r
tok3 <- tok1 %>% tokens_proximity(pattern = c("eu", "brussels"))
tok3
#> Tokens consisting of 2 documents.
#> text1 :
#>  [1] "turkish"   "president" "tayyip"    "erdogan"   ","         "in"       
#>  [7] "his"       "strongest" "comments"  "yet"       "on"        "the"      
#> [ ... and 26 more ]
#> 
#> text2 :
#>  [1] "eu"           "policymakers" "proposed"     "the"          "new"         
#>  [6] "agency"       "in"           "2021"         "to"           "stop"        
#> [11] "financial"    "firms"       
#> [ ... and 31 more ]
#> 
#> With proximity vector(s).
#> Pattern:  eu brussels
```

``` r
docvars(tok3, "proximity")
#> $text1
#>  [1] 39 39 39 39 39 39 39 39 39 39 39 39 39 39 39 39 39 39 39 39 39 39 39 39 39
#> [26] 39 39 39 39 39 39 39 39 39 39 39 39 39
#> 
#> $text2
#>  [1]  1  2  3  4  5  6  7  8  9 10  9  8  7  6  5  4  3  2  1  2  3  4  5  6  5
#> [26]  4  3  2  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
```

``` r
dfm(tok3) %>% dfm_lookup(dict1) %>% rowSums()
#>      text1      text2 
#> 0.02564103 0.45833333
```

Can we use [phrase](https://quanteda.io/reference/phrase.html)?

``` r
tok4 <- tok1 %>% tokens_proximity(pattern = phrase("Tayyip Erdogan"))
tok4
#> Tokens consisting of 2 documents.
#> text1 :
#>  [1] "turkish"   "president" "tayyip"    "erdogan"   ","         "in"       
#>  [7] "his"       "strongest" "comments"  "yet"       "on"        "the"      
#> [ ... and 26 more ]
#> 
#> text2 :
#>  [1] "eu"           "policymakers" "proposed"     "the"          "new"         
#>  [6] "agency"       "in"           "2021"         "to"           "stop"        
#> [11] "financial"    "firms"       
#> [ ... and 31 more ]
#> 
#> With proximity vector(s).
#> Pattern:  Tayyip Erdogan
```

``` r
docvars(tok4, "proximity")
#> $text1
#>  [1]  3  2  1  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22
#> [26] 23 24 25 26 27 28 29 30 31 32 33 34 35
#> 
#> $text2
#>  [1] 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44
#> [26] 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44 44
```

``` r
dfm(tok4) %>% dfm_lookup(dict1) %>% rowSums()
#>      text1      text2 
#> 0.04166667 0.04545455
```

# Similar functions

  - [quanteda](https://quanteda.io/): `quanteda::tokens_select(window)`,
    `quanteda::fcm()`
  - [qdap](https://trinker.github.io/qdap): `qdap::word_proximity()`,
    `qdap::weight()`
