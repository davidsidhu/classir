
<!-- README.md is generated from README.Rmd. Please edit that file, then run
     devtools::build_readme() -->

# classir

Note that this package was entirely coded by Claude AI based on
back-and-forth exchanges with me. Much of the text in the documentation
was also written by Claude AI. The package is also very much a work in
progress and hasn’t been thoroughly checked yet. It’s worth double
checking all its output.

This is a package for the typical kinds of operations psychologists (and
other researchers) do in R. It has functions for surveying datasets
(`look()`) and combining datasets (`add_values()`). It also has some
functions that are more geared towards psycholinguistic analyses but
might be helpful for others. This includes a function to simplify a
maximally complex random effects structure of a linear mixed effects
model (`converge()`) and a function that runs multiple approaches to
building regression models and reports all of the results
(`compare_methods()`). It also has a function to transcribe words into
IPA (`transcribe_phonemes()`).

I built this to streamline the operations I did often in R. I hope it
will be useful for you too!

The package is named after my lab, the Cognition, Language, Sound
Symbolism, Iconicity (CLaSSI) Lab. If this is helpful for you I’d love
to hear about it and can be reached at <david.sidhu@carleton.ca>

## Installation

``` r
install.packages("remotes")
remotes::install_github("davidsidhu/classir")
```

If R asks whether you want to install a package from source, answer
**no** — saying yes will try to compile it, which usually fails without
extra tools.

Some functions need packages that aren’t installed automatically:

``` r
install.packages(c("afex", "lmerTest", "VSURF", "readxl"))
```

## Start here

The four functions that will be helpful to most people are:

- **`look()`** — a survey of your data, one variable at a time
- **`check_data()`** — looks for anomalous cells
- **`prep()`** — prepares data for analyses
- **`add_values()`** — adds variables to your dataset, similar to
  VLOOKUP in Excel

The examples below use `demo_words`, a simulated dataset of 500 invented
words that comes with the package.

### `look()`

``` r
library(classir)

look(demo_words)
#> 
#> demo_words: 500 rows, 10 columns
#> 
#> (1) word <character>               (2) length <integer>
#>   Lvls  500                          Mean    5.67      
#>   e.g.  bartrust, basdreesh, bat     SD      2.31      
#>                                      Min     2.00      
#>                                      Median  5.00      
#>                                      Max     11.00     
#> 
#> (3) freq <numeric>              (4) old20 <numeric>   (5) aoa <numeric>  
#>   Mean    37.80                   Mean    2.80          Mean    9.36     
#>   SD      98.86                   SD      0.78          SD      2.44     
#>   Min     0.10                    Min     1.00          Min     2.89     
#>   Median  10.34                   Median  2.69          Median  9.43     
#>   Max     1,379.18                Max     5.12          Max     16.38    
#>   Skew    8.15 (bunched left)                           NA      40 (8.0%)
#>   Kurt    88.27 (peaked)                                                 
#> 
#> (6) concreteness <numeric>   (7) pos <character>     
#>   Mean    3.19                 noun       252 (50.4%)
#>   SD      0.92                 verb       147 (29.4%)
#>   Min     1.00                 adjective  98 (19.6%) 
#>   Median  3.23                 Noun       3 (0.6%)   
#>   Max     5.00                                       
#>   NA      12 (2.4%)                                  
#> 
#> (8) sound_symbolic <integer>    (9) iconicity <numeric>
#>   Mean    0.12                    Mean    3.26         
#>   SD      0.33                    SD      1.06         
#>   Min     0.00                    Min     -0.04        
#>   Median  0.00                    Median  3.30         
#>   Max     1.00                    Max     6.72         
#>   Skew    2.31 (bunched left)                          
#> 
#> (10) rt <numeric>              
#>   Mean    644.23               
#>   SD      123.85               
#>   Min     -999.00              
#>   Median  649.20               
#>   Max     835.80               
#>   Skew    -9.33 (bunched right)
#>   Kurt    121.72 (peaked)      
#> 
#> SUGGESTED CONVERSIONS
#> 
#>   pos has few distinct values and may be factor.
#>   Convert with:
#>     demo_words[c("pos")] <- lapply(demo_words[c("pos")], as.factor)
```

### `check_data()`

``` r
check_data(demo_words, id = "word")
#> 
#> demo_words: 500 rows, 10 columns
#> 
#> 8 issue(s) flagged
#> 
#> STRANGE CELLS
#>   freq                         4 value(s) beyond 5 SDs: 1379.18,  970.71,  679.41
#>   iconicity                    1 negative value(s) in an otherwise non-negative column: -0.04
#>   rt                           2 value(s) beyond 5 SDs: -999
#>   rt                           2 negative value(s) in an otherwise non-negative column: -999
#> 
#> CASE MISMATCH
#>   pos                          1 value(s) differ only in capitalisation
#> 
#> TYPE
#>   pos                          character with 4 distinct values -- factor?
#>   sound_symbolic               numeric with only 2 distinct value(s) -- factor?
#> 
#> SENTINEL VALUE
#>   rt                           contains -999, outside the rest of the range -- missing code?
#> 
#> SUGGESTED CONVERSIONS
#>   demo_words[c("pos")] <- lapply(demo_words[c("pos")], as.factor) 
#> 
#> All checks are heuristics -- a flag means worth a look, not wrong.
```

### `add_values()`

``` r
stimuli <- data.frame(word = demo_words$word[1:6])

add_values(stimuli, "word", demo_words, "word", c("freq", "aoa"))
#> Matched 6 of 6 selected row(s) (100.0%).
#> Added columns (class, missing):
#>   freq: numeric, 0 NA
#>   aoa: numeric, 0 NA
#>          word  freq   aoa
#> 1    bartrust  1.00 13.93
#> 2   basdreesh 63.34  4.98
#> 3         bat  3.78  8.11
#> 4  beabsweelt  0.44 11.36
#> 5 beachfleest  2.19 11.40
#> 6        beek 48.58  5.64
```

Students who would rather answer questions than write the call can use
`add_values_guided()`, which walks through the same steps and prints the
equivalent command at the end.

## All functions

### Surveying and cleaning

| Function | What it does |
|----|----|
| `look()` | A short labelled block for each variable in your dataset: mean, SD, range, level counts, missingness |
| `check_data()` | Scans for likely problems — wrong types, heavy missingness, constant columns, strange cells |
| `prep()` | Prepares a dataset for analysis: drops incomplete rows, keeps a subset of rows, sets reference levels, effects codes two-level factors, standardises numeric variables and renames columns |
| `prep_guided()` | An interactive version of `prep()` |

### Combining datasets

| Function | What it does |
|----|----|
| `add_values()` | Adds columns from another dataset, matching rows on a shared column |
| `add_values_guided()` | An interactive version of `add_values()` |

### Describing

| Function | What it does |
|----|----|
| `by_group()` | Mean and SD table of selected variables by one or two grouping variables |

### Regression and variable selection

| Function | What it does |
|----|----|
| `compare_methods()` | Runs several selection methods (correlation, regression, stepwise regression, best subsets regression, adaptive lasso and random forests) on the same data and reports all of the results in a grid |
| `alasso()` | Adaptive lasso, with ridge-derived penalty weights |
| `best_subsets_boot()` | Best-subsets regression by BIC, with bootstrap selection frequencies |
| `best_subsets_perm()` | A permutation null for the above |
| `best_subsets_report()` | Prints a `best_subsets_boot()` result |
| `converge()` | Works through a sequence of steps to get a non-converging `lmer` model to converge |

### Phonemes

| Function | What it does |
|----|----|
| `transcribe_phonemes()` | Looks words up in a pronunciation dictionary and optionally codes each phoneme |
| `word_to_ipa()` | Spelled words to IPA |
| `code_phonemes()` | IPA strings to one column per phoneme |
| `cmu_dict()` | Downloads and caches the CMU pronouncing dictionary |
| `build_cmu_ipa()` | Builds a word-to-IPA table from a CMUdict file |
| `english_phonemes` | The CMUdict English phoneme inventory, in IPA |

### Data

| Object | What it is |
|----|----|
| `demo_words` | A simulated dataset of 500 invented words, used in these examples |

## Comparing methods

`compare_methods()` runs several variable selection approaches on the
same data and shows where they agree.

``` r
compare_methods(demo_words, "iconicity",
                c("freq", "length", "old20", "aoa", "concreteness"),
                methods = c("cor", "lm", "step", "alasso", "subsets"),
                seed = 2026)
#> compare_methods() shows how robust an effect is across analysis methods. It is not for choosing which analysis to report based on the pattern of results.
#> Computing zero-order correlations...
#> Fitting multiple regression...
#> Running stepwise BIC (both directions)...
#> Running adaptive lasso...
#> Running best subsets...
#> Done.
#> 
#>   DEPENDENT VARIABLE: iconicity
#>   451 observations, 49 dropped   alpha = 0.05
#> 
#>              Corr Corr-BH Reg Stepwise aLasso Subsets
#> freq          −      −     −     −       −       −   
#> length        +      +                               
#> old20         +      +                               
#> aoa           +      +     +     +       +       +   
#> concreteness  +      +     +     +               +   
#> 
#>   + positive   − negative   ✓ selected   blank not selected
#>   Corr = Correlation   Corr-BH = Correlation (BH)   Reg = Regression   Stepwise = Stepwise BIC   aLasso = Adaptive lasso   Subsets = Best subsets
```

It is meant for examining how robust an effect is across analysis
methods. It is not for choosing which analysis to report based on the
pattern of results.

## A note on how this was made

This package was entirely created by Claude AI based on direction from
me.

## Credits

`cmu_dict()` uses the [CMU Pronouncing
Dictionary](https://github.com/cmusphinx/cmudict), Copyright (C)
1993–2014 Carnegie Mellon University.
