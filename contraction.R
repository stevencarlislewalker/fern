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
                 paste0("2016-07-08 ", startTime, ":00")))

dat$startTime <- as.POSIXct(dat$startTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
dat$duration  <-     seconds(dat$duration)

dat <-
    dat %>%
    mutate(endTime = startTime + duration) %>%
    mutate(durationRest = as.duration(endTime - lag(startTime))) %>%
    mutate(contractionID = seq_along(startTime)) %>%
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

durationPlot <-
    dat %>%
      ggplot() + theme_bw() +
      geom_segment(aes(x = startTime, xend = startTime,
                       y = 0,
                       yend = as.numeric(duration))) +
      scale_x_datetime("Contraction start time (EST)",
                       breaks = dateTicks,
                       labels = date_format("%H:%M", tz = "UTC")) +
      scale_y_continuous("Contraction duration (s)")

ggsave("intervalPlot.png", intervalPlot)
ggsave("durationPlot.png", durationPlot)
