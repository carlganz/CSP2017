# CSP2017
Shiny apps related to poster [*Using Shiny to Efficiently Process Survey Data*](https://ww2.amstat.org/meetings/csp/2017/onlineprogram/AbstractDetails.cfm?AbstractID=303391) at Conference for Statistical practice 2017 in Jacksonville

### Required packages

```{R}
install.packages('shiny')
install.packages('DT')
install.packages('rintrojs')
install.packages('tidyverse')
install.packages('stringdist')
install.packages('openxlsx')
```
For `openxlsx` to work on Windows you will need Rtools.

### Run Apps

To run the apps locally you'll first need to clone this github repo to your computer. This can be done with the `git2r` package.

```{R}
install.packages('git2r')
git2r::clone("https://github.com/carlganz/CSP2017.git",
                local_path = "path/to/folder")
                
# make folder working directory
setwd("path/to/folder")

# Run App A
shiny::runApp("App A")

# Run App B
shiny::runApp("App B")

# Run App C
shiny::runApp("App C")

```