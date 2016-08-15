## ---- echo = FALSE, message = FALSE--------------------------------------
library(knitr)
library(ggplot2)
source("fernFunctions.R")

## ---- echo = FALSE, message = FALSE, warning = FALSE---------------------
library(data.table)
library(dplyr)
library(fasttime)
library(lubridate)
library(ggplot2)
library(scales)
dat <-
    fread("contractionData.csv",
          colClasses = c("character", "integer")) %>%
      mutate(startTime = ifelse(startTime == "", NA,
                 paste0("2016-07-08 ", startTime, ":00"))) %>%
  mutate(contractionID = seq_along(startTime)) %>%
  select(contractionID, startTime, duration)

dat$startTime <- as.POSIXct(dat$startTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
dat$duration  <-     seconds(dat$duration)
knitr:::kable(rbind(head(dat), tail(dat)))

## ---- warning = FALSE----------------------------------------------------
dat <- dat %>%
  mutate(interval = as.duration(startTime - lag(startTime)))
knitr:::kable(dat[5:10,])

## ----intervalPlot, fig.width = 4, fig.height = 4-------------------------
intervalPlot <-
    makeIntervalPlot(dat) +
    scale_y_continuous("Inter-contraction interval (min)",
    expand = c(0, 1))
print(intervalPlot)

## ---- echo = FALSE-------------------------------------------------------
makeEquation(
  "initialModel",
  '$\\mbox{ \\Huge $ y = \\kappa_1 e^{- \\alpha x} + \\kappa_2 (1 - e^{- \\alpha x}) + \\epsilon $ }$')

## ---- echo = FALSE-------------------------------------------------------
makeSymbolTable(
  "mathSymbols",
  "\\mbox{\\Large \\begin{tabular}{ll} 
$y$ & inter-contraction interval \\\\ 
$x$ & contraction ID \\\\ 
$\\kappa_1$ & initial inter-contraction time \\\\
$\\kappa_2$ & final inter-contraction time \\\\
$\\alpha$ & parameter controlling the rate of decline from $\\kappa_1$ to $\\kappa_2$ \\\\
$\\epsilon$ & normally distributed error \\\\
\\end{tabular} }",
  2)

## ------------------------------------------------------------------------
datNoNA <- dat %>%
  as_data_frame %>%
  filter(!is.na(interval))
nonLinearFormula <- 
    as.numeric(interval) / 60 ~
    exp(k1) *      exp(- alpha * contractionID) + 
    exp(k2) * (1 - exp(- alpha * contractionID))
mod <- nls(nonLinearFormula, data = datNoNA, 
           start = list(alpha = 0.05, k1 = 3.5, k2 = 1.2),
           trace = TRUE)
print(summary(mod))

mod <- nlme(nonLinearFormula,
            data = datNoNA,
            fixed = alpha + k1 + k2 ~ 1,
            start = list(alpha = 0.05, k1 = 3.5, k2 = 1.2),
            weights = varExp(0.2, form = ~ as.numeric(contractionID)))

## ---- eval = FALSE, echo = FALSE-----------------------------------------
## ## Confidence interval on the estimated asymptotic inter-contraction interval
## k2Hat <- coef(mod)["k2"]
## k2SE <- sqrt(vcov(mod)["k2", "k2"])
## exp(k2Hat + c(lower = -1.96 * k2SE, estimate = 0, upper = 1.96 * k2SE))

## ---- eval = TRUE, echo = FALSE------------------------------------------
plotCurve <- function(alpha, k1, k2) {
    x <- 1:nrow(dat)
    logy <- log(exp(k1) * exp(- alpha * x) + exp(k2) * (1 - exp(- alpha * x)))
    plot(x, logy, type = "l", ylim = c(0, log(55)), lwd = 3, las = 1,
         xlab = "Contraction ID", ylab = "Inter-contraction interval (min)")
    with(dat, lines(contractionID, log(as.numeric(interval) / 60)))
    abline(h = c(k1, k2), lty = 2)
}
plotCurve(0.014, 2.56, -5.9)

## ----intervalPlotLog, fig.width = 4, fig.height = 4----------------------
intervalPlotLog <-
	makeIntervalPlot(dat) +
	scale_y_continuous("Inter-contraction interval (min)\n(log scale)",
                      trans = "log", 
                      breaks = c(2, 4, 8, 16, 32, 64),
		      expand = c(0, 0))
print(intervalPlotLog)

## ------------------------------------------------------------------------
datNoNA <- dat %>%
  as_data_frame %>%
  filter(!is.na(interval))
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

