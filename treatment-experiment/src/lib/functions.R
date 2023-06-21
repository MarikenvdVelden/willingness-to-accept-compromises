library(here)
library(kableExtra)
library(tidyverse)
library(tidycomm)
library(ggpubr)
library(cobalt)
library(scales)
library(broom)
library(margins)
library(ggstatsplot)
library(ggrepel)
library(patchwork)
library(qualtRics)
library(hrbrthemes)

fig_cols <- yarrr::piratepal(palette = "basel", 
             trans = .2)
fig_cols <- as.character(fig_cols[1:8])

api_key_fn <- here("data/raw-private/qualtrics_api_key.txt")
API <- read_file(api_key_fn) %>% trimws()

