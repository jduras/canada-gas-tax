
library(magrittr)
library(tidyverse)
library(tsibble)
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

source(str_c(here(), "/src/c01_scrape_and_clean_gas_data.R"))
source(str_c(here(), "/src/c02_scrape_and_clean_tax_data.R"))

