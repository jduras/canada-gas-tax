
library(magrittr)
library(tidyverse)
library(here)
library(readxl)
library(rvest)
library(jsonlite)
library(RecordLinkage)

source(str_c(here(), "/src/c01_scrape_and_clean_gas_data.R"))
source(str_c(here(), "/src/c02_scrape_and_clean_tax_data.R"))

