
library(magrittr)
library(tidyverse)
library(lubridate)
library(tsibble)
library(httr)
library(rvest)
library(readxl)
library(here)
library(jsonlite)
library(RecordLinkage)

theme_set(theme_minimal())

# first and last year in sample
yfst_daily <- 2016
ylst_daily <- 2018

yfst_weekly <- 2000
ylst_weekly <- 2018

# set API key for Google Maps Distance Matrix API call in c03_scrape_and_clean_distances_data.R
source(str_c(here(), "/src/c00_set_google_api_key.R"), echo = TRUE)

deparse.length <- 10000
source(str_c(here(), "/src/c01_scrape_and_clean_gas_data.R"), echo = TRUE, max.deparse.length = deparse.length)
source(str_c(here(), "/src/c02_scrape_and_clean_tax_data.R"), echo = TRUE, max.deparse.length = deparse.length)

