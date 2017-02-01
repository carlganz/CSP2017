[![Travis-CI Build Status](https://travis-ci.org/carlganz/CSP2017.svg?branch=master)](https://travis-ci.org/carlganz/CSP2017)

# CSP2017
This R package contains Shiny apps related to poster [*Using Shiny to Efficiently Process Survey Data*](https://ww2.amstat.org/meetings/csp/2017/onlineprogram/AbstractDetails.cfm?AbstractID=303391) at Conference for Statistical practice 2017 in Jacksonville

### Required packages

```{R}
install.packages('shiny')
install.packages('DT')
install.packages('tidyverse')
install.packages('stringdist')
install.packages('shinyjs')
install.packages('openxlsx')
```
For `openxlsx` to work on Windows you will need Rtools.

### Run Apps

To run the apps locally you'll first need to use `devtools` to install this package.

```{R}
devtools::install_github("carlganz/CSP2017")

library(CG.CSP17)

# Run App A
run_app("A") # or run_app(1)

# Run App B
run_app("B") # or run_app(2)

# Run App C
run_app("C") # or run_app(3)

```