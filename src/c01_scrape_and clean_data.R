
library(magrittr)
library(tidyverse)
library(rvest)

# first and last year in sample
yfst <- 2013
ylst <- 2017

# scrape gasoline price data from https://charting.kentgroupltd.com/
gas_tbl_raw <-
    crossing(yr = yfst:ylst,
             grade = c("Unleaded", "Premium"),
             market = c("Retail%20(Incl.%20Tax)", "Wholesale")) %>%
    mutate(link = str_c("https://charting.kentgroupltd.com/WPPS/", grade, "/", market, "/WEEKLY/", yr, "/", grade, "_", market, "_WEEKLY_", yr, ".htm"),
           webpage = map(link, read_html),
           data = map(webpage, ~ .x %>% html_table() %>% purrr::pluck(1)))

# inspect the gasoline price data
gas_tbl_raw$data[[1]] %>%
    glimpse()

gas_tbl_raw$data[[1]] %>%
    slice(-(1:2)) %>%
    glimpse()

gas_tbl_raw$data[[1]] %>%
    slice(-(1:2)) %>%
    as_tibble() %>%
    select(1:2) %>%
    tail(15)

# tidy the gasoline price data
gas_tbl <-
    gas_tbl_raw %>%
    mutate(data_clean = map(data, ~.x %>%
                                as_tibble() %>%
                                slice(-(1:2)) %>%
                                filter(!str_detect(X1, "\\([SVP]\\)")) %>%
                                filter(!str_detect(X1, "S-Simple")) %>%
                                filter(!str_detect(X1, "prices as of")) %>%
                                filter(!str_detect(X1, "To print")) %>%
                                filter(X1 != "") %>%
                                distinct(X1, .keep_all = TRUE) %>%
                                gather(key, value, -X1) %>%
                                filter(key != key[value == "Average"]) %>%
                                group_by(key) %>%
                                mutate(year = X1[str_detect(X1, "DATE")] %>% parse_number(),
                                       date = value[str_detect(X1, "DATE")] %>% str_c(as.character(year), "/", .) %>% as.Date(format = "%Y/%m/%d")) %>%
                                filter(value != X1) %>%
                                filter(X1 != "CANADA AVERAGE") %>%
                                filter(!is.na(date)) %>%
                                ungroup() %>%
                                filter(!str_detect(X1, "DATE")) %>%
                                select(date, city = X1, price = value)))

save(gas_tbl_raw, gas_tbl, file = "data/canada_gas_tax.Rdata")

gas_tbl %>%
    select(yr, grade, market, data_clean) %>%
    unnest() %$%
    unique(city)
