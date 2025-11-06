# Library necessary to reproduce 'Give a Litle, Take a Little' Paper
library(tidyverse)
library(ggstatsplot)
library(haven)
library(foreign)
library(broom)
library(here)
library(qualtRics)
#library(kableExtra)
library(cobalt)
library(margins)
library(patchwork)
library(hrbrthemes)

api_key_fn <- here("data/raw-private/qualtrics_api_key.txt")
API <- read_file(api_key_fn) %>% trimws()

fig_cols <- yarrr::piratepal(palette = "basel", 
                             trans = .2)
fig_cols <- as.character(fig_cols[1:9])
tmp <- as.character(yarrr::piratepal(palette = "pony", 
                                     trans = .2)[5])
fig_cols <- c(fig_cols, tmp)
tmp <- as.character(yarrr::piratepal(palette = "pony", 
                                     trans = .2)[8])
fig_cols <- c(fig_cols, tmp)
tmp <- as.character(yarrr::piratepal(palette = "pony", 
                                     trans = .2)[9])
fig_cols <- c(fig_cols, tmp)

regression_direct <- function(df, a, b){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(a, b, S1, S2, partner, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ a + b +
                                                   factor(S1) + S2 + factor(partner) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- tidy(allModels[[i]]) %>%
        mutate(x = term,
               y = depVarList[i],
               lower = estimate - (1.56 * std.error),
               upper = estimate + (1.56 * std.error)) %>%
        select(estimate, upper, lower, y, x, std.error, p.value)
    }
    else{
      tmp <- tidy(allModels[[i]]) %>%
        mutate(x = term,
               y = depVarList[i],
               lower = estimate - (1.56 * std.error),
               upper = estimate + (1.56 * std.error)) %>%
        select(estimate, upper, lower, y, x, std.error, p.value)
      m <- m %>%
        add_case(tmp)
    }
  }
  return(m)
}

regression_direct_party <- function(df, a, b){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(a, b, S2, partner, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ a + b +
                                                   S2 + factor(partner) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- tidy(allModels[[i]]) %>%
        mutate(x = term,
               y = depVarList[i],
               lower = estimate - (1.56 * std.error),
               upper = estimate + (1.56 * std.error)) %>%
        select(estimate, upper, lower, y, x)
    }
    else{
      tmp <- tidy(allModels[[i]]) %>%
        mutate(x = term,
               y = depVarList[i],
               lower = estimate - (1.56 * std.error),
               upper = estimate + (1.56 * std.error)) %>%
        select(estimate, upper, lower, y, x)
      m <- m %>%
        add_case(tmp)
    }
  }
  return(m)
}

regression_direct_explor <- function(df, a, b){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(a, b, S1, S2, partner,
                                c, HT3, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ a + b +
                                                   factor(S1) + S2 + factor(partner) +
                                                   c + HT3 +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- tidy(allModels[[i]]) %>%
        mutate(x = term,
               y = depVarList[i],
               lower = estimate - (1.56 * std.error),
               upper = estimate + (1.56 * std.error)) %>%
        select(estimate, upper, lower, y, x)
    }
    else{
      tmp <- tidy(allModels[[i]]) %>%
        mutate(x = term,
               y = depVarList[i],
               lower = estimate - (1.56 * std.error),
               upper = estimate + (1.56 * std.error)) %>%
        select(estimate, upper, lower, y, x)
      m <- m %>%
        add_case(tmp)
    }
  }
  return(m)
}

regression <- function(df, a, b){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(a, b, S1, S2, partner, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ a * b +
                                                   factor(S1) + S2 + factor(partner) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- tidy(allModels[[i]]) %>%
        mutate(x = term,
               y = depVarList[i],
               lower = estimate - (1.56 * std.error),
               upper = estimate + (1.56 * std.error)) %>%
        select(estimate, upper, lower, y, x, std.error, p.value)
    }
    else{
      tmp <- tidy(allModels[[i]]) %>%
        mutate(x = term,
               y = depVarList[i],
               lower = estimate - (1.56 * std.error),
               upper = estimate + (1.56 * std.error)) %>%
        select(estimate, upper, lower, y, x, std.error, p.value)
      m <- m %>%
        add_case(tmp)
    }
  }
  return(m)
}

regression_party <- function(df, a, b){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(a, b, S2, partner, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ a * b +
                                                   S2 + factor(partner) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- tidy(allModels[[i]]) %>%
        mutate(x = term,
               y = depVarList[i],
               lower = estimate - (1.56 * std.error),
               upper = estimate + (1.56 * std.error)) %>%
        select(estimate, upper, lower, y, x)
    }
    else{
      tmp <- tidy(allModels[[i]]) %>%
        mutate(x = term,
               y = depVarList[i],
               lower = estimate - (1.56 * std.error),
               upper = estimate + (1.56 * std.error)) %>%
        select(estimate, upper, lower, y, x)
      m <- m %>%
        add_case(tmp)
    }
  }
  return(m)
}

regression_explor <- function(df, a, b){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(a, b, S1, S2, partner, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ a * b +
                                                   factor(S1) + S2 + factor(partner) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "b", at = list(a = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, a)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "b", at = list(a = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, a)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

regression_ht1 <- function(df, compromise, outcome, b){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ outcome + compromise * b +
                                                   factor(S1) + S2 + factor(partner) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:14))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:14))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

regression_ht_oa <- function(df, compromise, outcome, b){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ outcome + compromise * b +
                                                   factor(S1) + S2 + factor(partner) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- tidy(allModels[[i]]) %>%
        mutate(x = term,
               y = depVarList[i]) %>%
        select(estimate, y, x, std.error, p.value)
    }
    else{
      tmp <- tidy(allModels[[i]]) %>%
        mutate(x = term,
               y = depVarList[i]) %>%
        select(estimate, y, x, std.error, p.value)
      m <- m %>%
        add_case(tmp)
    }
  }
  return(m)
}

regression_ht2 <- function(df, compromise, outcome, b){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ outcome + compromise * b +
                                                   factor(S1) + S2 + factor(partner) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:4))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:4))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

regression_ht3 <- function(df, compromise, outcome, b){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~  outcome + compromise * b +
                                                   factor(S1) + S2 + factor(partner) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:10))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:10))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

regression_ht4 <- function(df, compromise, outcome, b){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner, PT8, 
                                PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~  outcome + compromise * b +
                                                   factor(S1) + S2 + factor(partner) +
                                                   PT8 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

regression_ht5 <- function(df, compromise, outcome, b){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner, PT8, 
                                PT1_1, PT1_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ outcome + compromise * b +
                                                   factor(S1) + S2 + factor(partner) +
                                                   PT8 + PT1_1 + PT1_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:11))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:11))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

regression_ht6 <- function(df, compromise, outcome, b){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner, PT8, 
                                PT1_1, PT1_2,PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ outcome + compromise * b +
                                                   factor(S1) + S2 + factor(partner) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 1:5))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 1:5))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

regression_ht7 <- function(df, compromise, outcome, b){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~  outcome + compromise * b +
                                                   factor(S1) + S2 + factor(partner) +
                                                   PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:10))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:10))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

regression_ht8 <- function(df, compromise, outcome, b){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~  outcome + compromise * b +
                                                   factor(S1) + S2 + factor(partner) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "compromise", 
                           at = list(b = c("AfD", "CDU/CSU", "FDP", "Greens",
                                           "Left", "SPD", "Other party")))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "compromise", 
                             at = list(b = c("AfD", "CDU/CSU", "FDP", "Greens",
                                             "Left", "SPD", "Other party")))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

regression_ht1_party <- function(df, compromise, outcome, b){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S2, partner, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ outcome + compromise * b +
                                                   S2 + factor(partner) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:14))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:14))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

regression_ht2_party <- function(df, compromise, outcome, b){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S2, partner, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ outcome + compromise * b +
                                                   S2 + factor(partner) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:4))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:4))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

regression_ht3_party <- function(df, compromise, outcome, b){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S2, partner, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~  outcome + compromise * b +
                                                   S2 + factor(partner) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:10))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:10))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

pooled_regression <- function(df, a, b, issue){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(a, b, S1, S2, partner, issue, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ a * b +
                                                   factor(S1) + factor(partner) +
                                                   S2 + factor(issue) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- tidy(allModels[[i]]) %>%
        mutate(x = term,
               y = depVarList[i],
               lower = estimate - (1.56 * std.error),
               upper = estimate + (1.56 * std.error)) %>%
        select(estimate, upper, lower, y, x, std.error, p.value)
    }
    else{
      tmp <- tidy(allModels[[i]]) %>%
        mutate(x = term,
               y = depVarList[i],
               lower = estimate - (1.56 * std.error),
               upper = estimate + (1.56 * std.error)) %>%
        select(estimate, upper, lower, y, x, std.error, p.value)
      m <- m %>%
        add_case(tmp)
    }
  }
  return(m)
}

pooled_regression_party <- function(df, a, b, issue){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(a, b, S2, partner, issue, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ a * b +
                                                   factor(partner) +
                                                   S2 + factor(issue) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- tidy(allModels[[i]]) %>%
        mutate(x = term,
               y = depVarList[i],
               lower = estimate - (1.56 * std.error),
               upper = estimate + (1.56 * std.error)) %>%
        select(estimate, upper, lower, y, x)
    }
    else{
      tmp <- tidy(allModels[[i]]) %>%
        mutate(x = term,
               y = depVarList[i],
               lower = estimate - (1.56 * std.error),
               upper = estimate + (1.56 * std.error)) %>%
        select(estimate, upper, lower, y, x)
      m <- m %>%
        add_case(tmp)
    }
  }
  return(m)
}

pooled_regression_explor <- function(df, a, b, issue){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(a, b, S1, S2, partner, issue, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ a * b +
                                                   factor(S1) + factor(partner) +
                                                   S2 + factor(issue) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "b", at = list(a = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, a)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "b", at = list(a = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, a)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

pooled_regression_ht_oa <- function(df, compromise, outcome, b, issue){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner, issue, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ outcome + compromise * b +
                                                   factor(S1) + S2 + factor(partner) +
                                                   factor(issue) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- tidy(allModels[[i]]) %>%
        mutate(x = term,
               y = depVarList[i]) %>%
        select(estimate, y, x, std.error, p.value)
    }
    else{
      tmp <- tidy(allModels[[i]]) %>%
        mutate(x = term,
               y = depVarList[i]) %>%
        select(estimate, y, x, std.error, p.value)
      m <- m %>%
        add_case(tmp)
    }
  }
  return(m)
}

pooled_regression_ht1 <- function(df, compromise, outcome, b, issue){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner, issue, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ outcome + compromise * b +
                                                   factor(S1) + S2 + factor(partner) +
                                                   factor(issue) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:14))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:14))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

pooled_regression_ht2 <- function(df, compromise, outcome, b, issue){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner, issue, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ outcome + compromise * b +
                                                   factor(S1) + S2 + factor(partner)  +
                                                   factor(issue) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:4))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:4))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

pooled_regression_ht3 <- function(df, compromise, outcome, b, issue){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner, issue, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ outcome + compromise * b +
                                                   factor(S1) + S2 + factor(partner)  +
                                                   factor(issue) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:10))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:10))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

pooled_regression_ht4 <- function(df, compromise, outcome, b, issue){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner, issue, PT8, 
                                PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ outcome + compromise * b +
                                                   factor(S1) + S2 + factor(partner)  +
                                                   factor(issue) +
                                                   PT8 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

pooled_regression_ht5 <- function(df, compromise, outcome, b, issue){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner, issue, PT8, 
                                PT1_1, PT1_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ outcome + compromise * b +
                                                   factor(S1) + S2 + factor(partner) +
                                                   factor(issue) +
                                                   PT8 + PT1_1 + PT1_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:11))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:11))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

pooled_regression_ht6 <- function(df, compromise, outcome, b, issue){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner, issue, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ outcome + compromise * b +
                                                   factor(S1) + S2 + factor(partner)  +
                                                   factor(issue) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 1:5))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 1:5))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

pooled_regression_ht7 <- function(df, compromise, outcome, b, issue){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner, issue, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ outcome + compromise * b +
                                                   factor(S1) + S2 + factor(partner)  +
                                                   factor(issue) +
                                                   PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:10))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:10))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

pooled_regression_ht8 <- function(df, compromise, outcome, b, issue){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner, issue, 
                                PT8, PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ outcome + compromise * b +
                                                   factor(S1) + S2 + factor(partner)  +
                                                   factor(issue) + PT8 +
                                                   PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "compromise", 
                           at = list(b = c("AfD", "CDU/CSU", "FDP", "Greens",
                                           "Left", "SPD", "Other party")))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "compromise", 
                             at = list(b = c("AfD", "CDU/CSU", "FDP", "Greens",
                                             "Left", "SPD", "Other party")))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

pooled_regression_ht9 <- function(df, compromise, outcome, b, issue){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner, issue, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ compromise + outcome * b +
                                                   factor(S1) + S2 + factor(partner)  +
                                                   factor(issue) +
                                                   PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "outcome", at = list(b = 0:4))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "outcome", at = list(b = 0:4))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

pooled_regression_ht10 <- function(df, compromise, outcome, b, issue){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner, issue, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ compromise + outcome * b +
                                                   factor(S1) + S2 + factor(partner)  +
                                                   factor(issue) +
                                                   PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "outcome", at = list(b = 1:5))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "outcome", at = list(b = 1:5))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

pooled_regression_ht11 <- function(df, compromise, outcome, b, issue){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner, issue, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ compromise + outcome * b +
                                                   factor(S1) + S2 + factor(partner)  +
                                                   factor(issue) +
                                                   PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "outcome", at = list(b = 0:8))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "outcome", at = list(b = 0:8))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

pooled_regression_ht1_party <- function(df, compromise, outcome, b, issue){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S2, partner, issue, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ outcome + compromise * b +
                                                   S2 + factor(partner) +
                                                   factor(issue) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:14))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:14))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

pooled_regression_ht2_party <- function(df, compromise, outcome, b, issue){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S2, partner, issue, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ outcome + compromise * b +
                                                   S2 + factor(partner)  +
                                                   factor(issue) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:4))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:4))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

pooled_regression_ht3_party <- function(df, compromise, outcome, b, issue){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S2, partner, issue, PT8, 
                                PT1_1, PT1_2, PT3_2, D4, D7,
                                D9, D10) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ outcome + compromise * b +
                                                   S2 + factor(partner)  +
                                                   factor(issue) +
                                                   PT8 + PT1_1 + PT1_2 + PT3_2 +
                                                   factor(D4) + factor(D7) +
                                                   factor(D9) + factor(D10),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:10))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "compromise", at = list(b = 0:10))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
      m <- m %>%
        add_case(tmp)
      
    }
  }
  return(m)
}

