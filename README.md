
# edxtras

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Pkg-Version](https://badgen.net/static/Pkg-Version/1.2.0/blue?icon=[gitlab])]()
[![R-Version](https://badgen.net/static/R-Version/>=3.5/blue?icon=[gitlab])]()
[![R-CMD-Check](https://badgen.net/static/R-CMD-Check/Passing/green?icon=[gitlab])]()
[![coverage](https://badgen.net/static/Test-Coverage/81-percent/green)]()

<!-- badges: end -->

This package supports data validation and quality control monitoring
for electronic data exchange (EDX) programs. The functions herein were designed to help validate
data sets coming from laboratories and health clinics (e.g., hospitals), though can be applied anywhere routine data validation is implemented in R.

## Installation
This package is not currently on CRAN.
You can install edxtras like so:

``` r
library(remotes)
remotes::install_github("peernisse/edxtras")

```
## Background
The `edxtras` package has been developed at the Utah Department of Health and Human Services (DHHS), Division of Population Health Informatics Program (DPHIP). Functions in the package are developed to aid in ongoing validation of laboratory and health clinic data reported to assess reporter compliance with public health reporting regulations, namely, Utah Admin. Code R386-702, the Communicable Disease Rule.

## Contribution

- Browse the source code at [this link](https://github.com/peernisse/edxtras)
- You can report a bug or issues at [this link](https://github.com/peernisse/edxtras/issues/new)
- You can submit a pull request at [this link](https://github.com/peernisse/edxtras/compare)

## Examples
<!--
This is a basic example to run completeness and validity validation on a dataframe using the default QA validRules:

<details>
    <summary>Example Code</summary>

``` r
library(edxtras)
print(validRules()) # shows the default rules

# Simple example ----

## validRules() is used by makeValidData

df <- data.frame(
    'myindex' = c('Bob','Bob','Larry'),
    'lastname' = c('AAA','BBB','CCC'),
    'gender' = c('Male','Female','M A L E'), 
    'norule' = c('A','B','C')
)
makeValidData(df, indexCol = 'myindex', filtRep = 'Bob')
makeValidData(df, indexCol = 'myindex', filtRep = 'Larry')
```
</details>
<br/>
-->
This is an example of using several `edxtras` validation functions with a 10M row DF:

<details>
  <summary>Example Code</summary>

``` r
# Make dataframe of 10 million rows and use custom rules ----

n <- 10000000
set.seed(42)
myDF <- data.frame(
    'numbers' = sample(c(84121,85236,65987,NA_real_), n, replace = TRUE),
    'chars' = sample(c('word','sentence of words', 'Bob', '42', '1111'), n, replace = TRUE),
    'dates' = sample(seq.Date(as.Date('2021-01-01'), Sys.Date() + 100, 'days'), n, replace = TRUE)
)
head(myDF)
str(myDF)

# Hypothetical validation requirements ----
    ## Find missing numbers in "numbers"
    ## Find numeric characters in "chars"
    ## Find dates older than 2 years or any dates in the future in "dates"

# Set up named list of rules ----

rules <- list(
    'numbers' = function(x) is.na(as.numeric(x)),
    'chars' = function(x) grepl('[^0-9]', x),
    'dates' = function(x) {!is.na(as.POSIXct(x, format = "%m/%d/%Y")) & 
            as.POSIXct(x, format = "%m/%d/%Y") <= as.POSIXct(Sys.Date()) & 
            as.POSIXct(x, format = "%m/%d/%Y") > as.POSIXct(Sys.Date() - 2*365)}
)

# Create validation rules list with validRules() ----

?edxtras::validRules
myNewRules <- validRules(rules, ruleset = 'myCustomRules')
print(myNewRules)
as.data.frame(myNewRules)

# Validate a single column returns percent TRUE----

?edxtras::validateColumn
validateColumn(df = myDF, col = 'numbers', rulesObj = myNewRules, ruleset = 'myCustomRules')
validateColumn(df = myDF, col = 'chars', rulesObj = myNewRules, ruleset = 'myCustomRules')
validateColumn(df = myDF, col = 'dates', rulesObj = myNewRules, ruleset = 'myCustomRules')

# Validate the dataframe returns percent TRUE in each column (NA if no rule/column name match) ----

?edxtras::validateDF
validateDF(df = myDF, rulesObj = myNewRules, ruleset = 'myCustomRules')
system.time(
    validateDF(df = myDF, rulesObj = myNewRules, ruleset = 'myCustomRules')
)

# Validate dataframe for completeness and validity ----
## Add a grouping column to loop on

myDF$Groups <- c(rep('A', n*.05), rep('B', n*.05), rep('C', n*.9)) 
head(myDF)
table(myDF$Groups)

?edxtras::makeValidData
makeValidData(df = myDF, indexCol = 'Groups', filtRep = 'A', 
    rulesObj = myNewRules, ruleset = 'myCustomRules'
)

system.time(
    makeValidData(df = myDF, indexCol = 'Groups', filtRep = 'A', 
        rulesObj = myNewRules, ruleset = 'myCustomRules'
    )
)

## With for loop ----

output <- data.frame()

system.time(
    for(i in unique(myDF$Groups)){
        out <- makeValidData(df = myDF, indexCol = 'Groups', filtRep = i, 
            rulesObj = myNewRules, ruleset = 'myCustomRules'
        )
        
        output <- rbind(output, out)
    }
)    
output

## With purrr::map_df() ----

grps <- unique(myDF$Groups)
system.time(
    output <- purrr::map_df(seq_along(grps), ~ 
        makeValidData(df = myDF, indexCol = 'Groups', filtRep = grps[[.x]], 
            rulesObj = myNewRules, ruleset = 'myCustomRules'
        )                   
    )
)
output

```
</details>

## Contact Info

This package is maintained by Utah DHHS DPHIP Staff

[EDX Team](mailto:edx@utah.gov)

