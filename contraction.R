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
  mutate(endTime = startTime + duration)

dateTicks <-
    paste0("2016-07-08 ", c("6:00", "9:00", "12:00", "15:00", "18:00"), ":00") %>%
      as.POSIXct(format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

## plot raw data
pp <-
    dat %>%
      ggplot() + theme_bw() +
      geom_segment(aes(x = startTime, xend = startTime,
                       y = 0,
                       yend = as.numeric(duration))) +
      scale_x_datetime("Contraction start time (EST)",
                       breaks = dateTicks,
                       labels = date_format("%H:%M", tz = "UTC")) +
      scale_y_continuous("Contraction duration (s)")
print(pp)
ggplotly(pp)

pp <-
    dat %>%
      ggplot() + theme_bw() +
      geom_segment(aes(x = startTime, xend = endTime,
                       y = as.numeric(duration), yend = as.numeric(duration)))

dat %>%

