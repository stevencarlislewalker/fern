---
title: "Fern Hazel Timms Walker"
author: "Steve Walker"
date: "July 16, 2016"
output: 
  html_document:
    toc: true
    theme: 'united'
---

## The birth of Fern

On July 8, 2016 at 11:08pm, my daughter was born.  She is adorable.
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


|   | contractionID|startTime           | duration|
|:--|-------------:|:-------------------|--------:|
|1  |             1|2016-07-08 06:15:00 |       NA|
|2  |             2|NA                  |       NA|
|3  |             3|NA                  |       NA|
|4  |             4|2016-07-08 08:04:00 |       NA|
|5  |             5|2016-07-08 08:31:00 |      66S|
|6  |             6|2016-07-08 08:42:00 |      20S|
|84 |            84|2016-07-08 18:57:00 |      56S|
|85 |            85|2016-07-08 19:00:00 |      59S|
|86 |            86|2016-07-08 19:02:00 |      63S|
|87 |            87|2016-07-08 19:05:00 |      50S|
|88 |            88|2016-07-08 19:09:00 |       NA|
|89 |            89|2016-07-08 19:11:00 |       NA|

Missing data are inevitable.

Of primary interest are the inter-contraction intervals.

```r
dat <- dat %>%
  mutate(interval = as.duration(startTime - lag(startTime)))
knitr:::kable(dat[5:10,])
```



|   | contractionID|startTime           | duration|            interval|
|:--|-------------:|:-------------------|--------:|-------------------:|
|5  |             5|2016-07-08 08:31:00 |      66S| 1620s (~27 minutes)|
|6  |             6|2016-07-08 08:42:00 |      20S|  660s (~11 minutes)|
|7  |             7|2016-07-08 08:51:00 |      20S|   540s (~9 minutes)|
|8  |             8|2016-07-08 09:05:00 |      25S|  840s (~14 minutes)|
|9  |             9|2016-07-08 09:22:00 |      47S| 1020s (~17 minutes)|
|10 |            10|2016-07-08 09:36:00 |      31S|  840s (~14 minutes)|

## Exploratory graphs


```r
intervalPlot <-
    dat %>%
    filter(!is.na(interval)) %>%
    ggplot() + theme_bw() + 
    geom_line(aes(contractionID, as.numeric(interval) / 60)) +
    geom_ribbon(aes(x = contractionID, ymin = 3, ymax = 4),
                alpha = 0.4) +
    scale_x_continuous("Contraction ID") +
   scale_y_continuous("Inter-contraction interval (min)\n(log scale)",
                      trans = "log", 
                      breaks = c(2, 4, 8, 16, 32, 64))
print(intervalPlot)
```

![plot of chunk intervalPlot](figure/intervalPlot-1.png)

The inter-contraction intervals are more variable and generally larger
near the beginning of labour.  It looks like the intervals might
converge on 3-4 minutes --- shown by the horizontal band --- just
before we get ready to go to the hospital!  But what kind of model
might determine such convergence?

## An initial model

It looks like the mean inter-contraction interval starts off somewhere
near 30 minutes and then declines to asymptotically approach 3-4
minutes.  A model with these characteristics is given by the
following.


![initial model](initialModel.png)

where $y$ and $x$ are the inter-contraction interval and contraction
ID, $\kappa_1$ and $\kappa_2$ are the initial and final
inter-contraction times, $\alpha$ is a parameter controlling the rate
of decline from $\kappa_1$ to $\kappa_2$, and $\epsilon$ is normally
distributed error.

## Fitting the model


```r
plotCurve <- function(alpha, k1, k2) {
    x <- 1:nrow(dat)
    logy <- log(exp(k1) * exp(- alpha * x) + exp(k2) * (1 - exp(- alpha * x)))
    plot(x, logy, type = "l", ylim = c(0, log(55)), lwd = 3, las = 1,
         xlab = "Contraction ID", ylab = "Inter-contraction interval (min)")
    with(dat, lines(contractionID, log(as.numeric(interval) / 60)))
    abline(h = c(k1, k2), lty = 2)
}
plotCurve(0.014, 2.56, -5.9)

datNoNA <- dat %>%
  as_data_frame %>%
  filter(!is.na(interval))
nonLinearFormula <- 
    log(as.numeric(interval) / 60) ~
    log(exp(k1) *      exp(- alpha * contractionID) + 
        exp(k2) * (1 - exp(- alpha * contractionID)))
mod <- nls(nonLinearFormula, data = datNoNA, 
           start = list(alpha = 0.05, k1 = 3.5, k2 = 1.2),
           trace = TRUE)
do.call(plotCurve, as.list(coef(mod)))
summary(mod)

## confidence interval on the estimated asymptotic inter-contraction interval
k2Hat <- coef(mod)["k2"]
k2SE <- sqrt(vcov(mod)["k2", "k2"])
exp(k2Hat + c(lower = -1.96 * k2SE, estimate = 0, upper = 1.96 * k2SE))
```
