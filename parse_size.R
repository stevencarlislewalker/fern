library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)
library(stringr)
library(ggplot2)
library(reshape2)

xx = readLines('size') %>%
    sub(pattern = '- ', replacement = 'date: ') %>%
    strsplit(';') %>%
    lapply(
        function(x) {
            x %>%
              strsplit(': ') %>%
              lapply(rev) %>%
              lapply(as.list) %>%
              lapply(do.call, what = setNames) %>%
              unlist
        }
    ) %>%
    lapply(as.list) %>%
    lapply(as.data.frame, stringsAsFactors = FALSE) %>%
    rbindlist(fill = TRUE) %>%
    mutate(
        month = date %>%
          substr(1, 3) %>%
          match(month.abb) %>%
          str_pad(2, pad = '0'),
        day = date %>%
          str_extract('((?:[0-9])+)') %>%
          str_pad(2, pad = '0'),
        year = date %>%
          strsplit(', ') %>%
          sapply('[[', 2)
    ) %>%
    mutate(
        date = ymd(paste0(year, month, day))
    ) %>%
    mutate(
        lbs_oz = str_extract_all(weight, '((?:[0-9])+)')
    ) %>%
    mutate(
        lbs = sapply(lbs_oz, '[[', 1),
        oz = sapply(lbs_oz, '[[', 2)
    ) %>%
    select(-lbs_oz) %>%
    mutate(weight = (as.numeric(oz) / 16) + as.numeric(lbs)) %>%
    select(-lbs, -oz) %>%
    mutate(
        height = as.numeric(str_extract(height, '((?:[0-9])+)')),
        head = as.numeric(str_extract(head, '((?:[0-9])+)'))
    )

p = ggplot(xx, aes(date)) + theme_bw() + geom_line() + geom_point()
p + aes(y = weight)
p + aes(y = height)
p + aes(y = head)

xx %>%
  melt(id.vars = c('date', 'year', 'month', 'day')) %>%
  ggplot(aes(date, value)) +
    facet_wrap(~ variable, scales = 'free', ncol = 2) +
    geom_point() +
    geom_line() +
    theme_bw()

ggplot(xx, aes(log(weight), log(height))) +
  geom_point() +
  geom_smooth(method=lm) +
  theme_bw()
