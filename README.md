---
title: "Fern Hazel Timms Walker"
author: "Steve Walker"
date: "July 16, 2016"
output: 
  html_document:
    toc: true
    theme: 'united'
---


```
## Warning in file(filename, "r", encoding = encoding): cannot open file
## 'fernFunctions.R': No such file or directory
```

```
## Error in file(filename, "r", encoding = encoding): cannot open the connection
```

## The birth of Fern

On July 8, 2016 at 11:09pm, my daughter was born.  She is adorable.
Here she is on July 9.

![Fern](FernWalker.png)

## The contraction data

At about 6:15am on July 8, Fern's Mom started having contractions.
Our midwives said to call them once the contractions were consistently
3-4 minutes apart for at least one hour, and that the contractions
themselves were between 45-90s in duration.  I started collecting
data.

![Raw Data](rawData.png)

Here are the first (and last) six observations that I recorded.


```
## Error in fread("data/ContractionData.csv", colClasses = c("character", : File 'data/ContractionData.csv' does not exist. Include one or more spaces to consider the input a system command.
```

```
## Error in as.POSIXct(dat$startTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"): object 'dat' not found
```

```
## Error in period(second = x): object 'dat' not found
```

```
## Error in head(dat): object 'dat' not found
```

Missing data are inevitable.

Of primary interest are the inter-contraction intervals.

```r
dat <- dat %>%
  mutate(interval = as.duration(startTime - lag(startTime)))
```

```
## Error in eval(expr, envir, enclos): object 'dat' not found
```

```r
knitr:::kable(dat[5:10,])
```

```
## Error in inherits(x, "list"): object 'dat' not found
```
## An initial model

The first step in model building is to look at the data.


```r
intervalPlot <-
    makeIntervalPlot(dat) +
    scale_y_continuous("Inter-contraction interval (min)",
    expand = c(0, 1))
```

```
## Error in eval(expr, envir, enclos): could not find function "makeIntervalPlot"
```

```r
print(intervalPlot)
```

```
## Error in print(intervalPlot): object 'intervalPlot' not found
```

The inter-contraction intervals are more variable and generally larger
near the beginning of labour (small contraction IDs).  It looks like
the intervals might converge on 3-4 minutes --- shown by the
horizontal band --- just before we get ready to go to the hospital!
But what kind of model might determine such convergence?

It looks like the mean inter-contraction interval starts off somewhere
near 30 minutes and then declines to asymptotically approach 3-4
minutes.  A model with these characteristics is given by the
following.


```
## Error in eval(expr, envir, enclos): could not find function "makeLatex"
```
![initial model](initialModel.png)
where,

```
## Error in eval(expr, envir, enclos): could not find function "makeLatex"
```
![math symbols](mathSymbols.png)

The first term represents the decay away from the initial
inter-contraction time, the second term represents the decay towards
the final inter-contraction time, and the last term is normally
distributed error.  We can fit this model to the contraction data
using the `nls` function in `R`.

```r
datNoNA <- dat %>%
  as_data_frame %>%
  filter(!is.na(interval))
```

```
## Error in eval(expr, envir, enclos): object 'dat' not found
```

```r
nonLinearFormula <- 
    as.numeric(interval) / 60 ~
    exp(k1) *      exp(- alpha * contractionID) + 
    exp(k2) * (1 - exp(- alpha * contractionID))
mod <- nls(nonLinearFormula, data = datNoNA, 
           start = list(alpha = 0.05, k1 = 3.5, k2 = 1.2),
           trace = TRUE)
```

```
## Error in nls(nonLinearFormula, data = datNoNA, start = list(alpha = 0.05, : object 'datNoNA' not found
```

```r
print(summary(mod))
```

```
## Error in summary(mod): object 'mod' not found
```

The model parameters converge quite readily, given the choice of
initial estimates that I got from trial and error.




```
## Error in nrow(dat): object 'dat' not found
```

## Accounting for Heterogeneous errors

One of the problems with ...


```r
intervalPlotLog <-
	makeIntervalPlot(dat) +
	scale_y_continuous("Inter-contraction interval (min)\n(log scale)",
                      trans = "log", 
                      breaks = c(2, 4, 8, 16, 32, 64),
		      expand = c(0, 0))
```

```
## Error in eval(expr, envir, enclos): could not find function "makeIntervalPlot"
```

```r
print(intervalPlotLog)
```

```
## Error in print(intervalPlotLog): object 'intervalPlotLog' not found
```


```r
datNoNA <- dat %>%
  as_data_frame %>%
  filter(!is.na(interval))
```

```
## Error in eval(expr, envir, enclos): object 'dat' not found
```

```r
nonLinearFormula <- 
    log(as.numeric(interval) / 60) ~
    log(exp(k1) *      exp(- alpha * contractionID) + 
        exp(k2) * (1 - exp(- alpha * contractionID)))
mod <- try({
nls(nonLinearFormula, data = datNoNA, 
           start = list(alpha = 0.05, k1 = 3.5, k2 = 1.2),
           trace = TRUE)
do.call(plotCurve, as.list(coef(mod)))
summary(mod)
})
mod
```

```
## [1] "Error in nls(nonLinearFormula, data = datNoNA, start = list(alpha = 0.05,  : \n  object 'datNoNA' not found\n"
## attr(,"class")
## [1] "try-error"
## attr(,"condition")
## <simpleError in nls(nonLinearFormula, data = datNoNA, start = list(alpha = 0.05,     k1 = 3.5, k2 = 1.2), trace = TRUE): object 'datNoNA' not found>
```

This is a stupid error.  The nonlinear solver chases unrealistically
low asymptotic inter-contraction intervals.
