# Library necessary to reproduce 'Give a Litle, Take a Little' Paper
library(tidyverse)
library(broom)
library(here)
library(kableExtra)
library(margins)
library(patchwork)

api_key_fn <- here("data/raw-private/qualtrics_api_key.txt")
API <- read_file(api_key_fn) %>% trimws()

fig_cols <- yarrr::piratepal(palette = "basel", 
                             trans = .2)
fig_cols <- as.character(fig_cols[1:8])

regression_direct <- function(df, a, b){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(a, b, S1, S2, partner) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ a + b +
                                                   factor(S1) + S2 + factor(partner),
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
  indepVarList <- df %>% select(a, b, S1, S2, partner) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ a * b +
                                                   factor(S1) + S2 + factor(partner),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "a", at = list(b = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "a", at = list(b = 0:1))) %>%
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

regression_ht1 <- function(df, compromise, outcome, b){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ compromise + outcome * b +
                                                   factor(S1) + S2 + factor(partner),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "outcome", at = list(b = 0:14))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "outcome", at = list(b = 0:14))) %>%
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

regression_ht2 <- function(df, compromise, outcome, b){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ compromise + outcome * b +
                                                   factor(S1) + S2 + factor(partner),
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

regression_ht3 <- function(df, compromise, outcome, b){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ compromise + outcome * b +
                                                   factor(S1) + S2 + factor(partner),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "outcome", at = list(b = 0:10))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "outcome", at = list(b = 0:10))) %>%
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
  indepVarList <- df %>% select(a, b, S1, S2, partner, issue) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ a * b +
                                                   factor(S1) + factor(partner) +
                                                   S2 + factor(issue),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "a", at = list(b = 0:1))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "a", at = list(b = 0:1))) %>%
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

pooled_regression_ht1 <- function(df, compromise, outcome, b, issue){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner, issue) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ compromise + outcome * b +
                                                   factor(S1) + S2 + factor(partner) +
                                                   factor(issue),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "outcome", at = list(b = 0:14))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "outcome", at = list(b = 0:14))) %>%
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
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner, issue) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ compromise + outcome * b +
                                                   factor(S1) + S2 + factor(partner)  +
                                                   factor(issue),
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

pooled_regression_ht3 <- function(df, compromise, outcome, b, issue){
  
  depVarList <- df %>% select(matches("DV[123]"))
  indepVarList <- df %>% select(compromise, outcome, b, S1, S2, partner, issue) 
  allModels <- apply(depVarList,2,function(xl)lm(xl ~ compromise + outcome * b +
                                                   factor(S1) + S2 + factor(partner)  +
                                                   factor(issue),
                                                 data= indepVarList))
  depVarList <- df %>% select(matches("DV[123]")) %>% colnames()
  
  for(i in 1:length(depVarList)){
    if(i==1){
      m <- summary(margins(allModels[[i]], variables = "outcome", at = list(b = 0:10))) %>%
        mutate(y = depVarList[i],
               lower = AME - (1.56 * SE),
               upper = AME + (1.56 * SE)) %>%
        select(AME, upper, lower, y, b)
    }
    else{
      tmp <- summary(margins(allModels[[i]], variables = "outcome", at = list(b = 0:10))) %>%
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
