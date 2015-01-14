## Introduction
This is an R package providing functions to compute standardaized regression coefficients - called betas - for different kind of linear models.

## Using the package
The use is as simple as 
```r
betas.lm(fit)   # fit is a linear model
```
or 
```r
betas.lmr(fit.robust)   # fit.robust is a robust linear model
```

## Installation
Installing from CRAN
```r
install.packages("betas")
library(betas)
````
or installing from GitHub
```r
library(devtools)
install_github("andreaphsz/betas.git")
library(betas)
```

## Included data set
The package includes a subset of the PISA 2012 students data set.
