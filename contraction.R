library(data.table)
library(dplyr)
library(fasttime)
library(lubridate)
library(ggplot2)
library(scales)
library(plotly)
dat <-
    fread("contractionData.csv",
          colClasses = c("character", "integer")) %>%
      mutate(startTime = ifelse(startTime == "", NA,
                 paste0("2016-07-08 ", startTime, ":00"))) %>%
     mutate(contractionID = seq_along(startTime))

dat$startTime <- as.POSIXct(dat$startTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
dat$duration  <-     seconds(dat$duration)

dat <-
    dat %>%
    mutate(endTime = startTime + duration) %>%
    mutate(durationRest = as.duration(endTime - lag(startTime))) %>%
    mutate(durationRatio = as.numeric(duration) / as.numeric(durationRest)) %>%
    mutate(interval = as.duration(startTime - lag(startTime)))

intervalPlot <-
    dat %>%
    ggplot() + theme_bw() + 
    geom_line(aes(contractionID, as.numeric(interval) / 60)) +
    geom_ribbon(aes(x = contractionID, ymin = 3, ymax = 4),
                alpha = 0.4) +
    geom_smooth(aes(contractionID, as.numeric(interval) / 60),
                method = "lm", formula = y ~ log(x)) +
    scale_x_continuous("Contraction ID") +
    scale_y_continuous("Inter-contraction interval (min)")

dateTicks <-
    paste0("2016-07-08 ", c("6:00", "9:00", "12:00", "15:00", "18:00"), ":00") %>%
      as.POSIXct(format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

timePlot <-
    dat %>%
      ggplot() + theme_bw() +
      geom_segment(aes(x = startTime, xend = startTime,
                       y = 0,
                       yend = as.numeric(duration))) +
      scale_x_datetime("Contraction start time (EST)",
                       breaks = dateTicks,
                       labels = date_format("%H:%M", tz = "UTC")) +
      scale_y_continuous("Contraction duration (s)")


durationPlot <-
    dat %>%
    ggplot() + theme_bw() + 
    geom_line(aes(contractionID, as.numeric(duration))) +
    geom_ribbon(aes(x = contractionID, ymin = 45, ymax = 90),
                alpha = 0.4) +
    scale_x_continuous("Contraction ID") +
    scale_y_continuous("Contraction duration (s)")

ggsave("timePlot.png", timePlot)
ggsave("intervalPlot.png", intervalPlot)
ggsave("durationPlot.png", durationPlot)

plotCurve <- function(alpha, k1, k2) {
    x <- 1:nrow(dat)
    y <- exp(k1) * exp(- alpha * x) + exp(k2) * (1 - exp(- alpha * x))
    plot(x, y, type = "l", ylim = c(0, 55), lwd = 3, las = 1,
         xlab = "Contraction ID", ylab = "Inter-contraction interval (min)")
    with(dat, lines(contractionID, as.numeric(interval) / 60))
    abline(h = c(exp(k1), exp(k2)), lty = 2)
}
plotCurve(0.05, 3.5, 1.2)

nonLinearFormula <- 
    as.numeric(interval) / 60 ~
    exp(k1) * exp(- alpha * contractionID) + exp(k2) * (1 - exp(- alpha * contractionID))
mod <- nls(nonLinearFormula, data = dat, start = list(alpha = 0.05, k1 = 3.5, k2 = 1.2))
do.call(plotCurve, as.list(coef(mod)))
summary(mod)

## confidence interval on the estimated asymptotic inter-contraction interval
k2Hat <- coef(mod)["k2"]
k2SE <- sqrt(vcov(mod)["k2", "k2"])
exp(k2Hat + c(lower = -1.96 * k2SE, estimate = 0, upper = 1.96 * k2SE))
