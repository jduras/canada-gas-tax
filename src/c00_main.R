
library(magrittr)
library(tidyverse)
library(tsibble)
library(rvest)
library(here)
library(jsonlite)
library(RecordLinkage)

theme_set(theme_minimal())

source(str_c(here(), "/src/c01_scrape_and_clean_gas_data.R"))
source(str_c(here(), "/src/c02_scrape_and_clean_tax_data.R"))

