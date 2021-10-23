# Library necessary to reproduce 'Give a Litle, Take a Little' Paper
library(tidyverse)
library(ggstatsplot)
library(haven)
library(foreign)
library(broom)
library(here)
library(qualtRics)

api_key_fn <- here("data/raw-private/qualtrics_api_key.txt")
API <- read_file(api_key_fn) %>% trimws()