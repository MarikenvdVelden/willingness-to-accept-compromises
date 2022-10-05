# Library necessary to reproduce 'Give a Litle, Take a Little' Paper
library(tidyverse)
library(ggstatsplot)
library(haven)
library(foreign)
library(broom)
library(here)
library(qualtRics)
library(kableExtra)
library(cobalt)
library(margins)
library(patchwork)

api_key_fn <- here("data/raw-private/qualtrics_api_key.txt")
API <- read_file(api_key_fn) %>% trimws()

fig_cols <- yarrr::piratepal(palette = "basel", 
                             trans = .2)
fig_cols <- as.character(fig_cols[1:8])

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


theme_ipsum <- function(base_family="Arial Narrow", base_size = 11.5,
                        plot_title_family=base_family, plot_title_size = 18,
                        plot_title_face="bold", plot_title_margin = 10,
                        subtitle_family=base_family, subtitle_size = 12,
                        subtitle_face = "plain", subtitle_margin = 15,
                        strip_text_family = base_family, strip_text_size = 12,
                        strip_text_face = "plain",
                        caption_family = base_family, caption_size = 9,
                        caption_face = "italic", caption_margin = 10,
                        axis_text_size = base_size,
                        axis_title_family = subtitle_family, axis_title_size = 9,
                        axis_title_face = "plain", axis_title_just = "rt",
                        plot_margin = margin(30, 30, 30, 30),
                        grid_col = "#cccccc", grid = TRUE,
                        axis_col = "#cccccc", axis = FALSE, ticks = FALSE) {
  
  ret <- ggplot2::theme_minimal(base_family=base_family, base_size=base_size)
  
  ret <- ret + theme(legend.background=element_blank())
  ret <- ret + theme(legend.key=element_blank())
  
  if (inherits(grid, "character") | grid == TRUE) {
    
    ret <- ret + theme(panel.grid=element_line(color=grid_col, size=0.2))
    ret <- ret + theme(panel.grid.major=element_line(color=grid_col, size=0.2))
    ret <- ret + theme(panel.grid.minor=element_line(color=grid_col, size=0.15))
    
    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + theme(panel.grid.major.x=element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + theme(panel.grid.major.y=element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.x=element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.y=element_blank())
    }
    
  } else {
    ret <- ret + theme(panel.grid=element_blank())
  }
  
  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + theme(axis.line=element_line(color="#2b2b2b", size=0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x=element_blank())
      } else {
        ret <- ret + theme(axis.line.x=element_line(color=axis_col, size=0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y=element_blank())
      } else {
        ret <- ret + theme(axis.line.y=element_line(color=axis_col, size=0.15))
      }
    } else {
      ret <- ret + theme(axis.line.x=element_line(color=axis_col, size=0.15))
      ret <- ret + theme(axis.line.y=element_line(color=axis_col, size=0.15))
    }
  } else {
    ret <- ret + theme(axis.line=element_blank())
  }
  
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
    ret <- ret + theme(axis.ticks.x = element_blank())
    ret <- ret + theme(axis.ticks.y = element_blank())
  } else {
    ret <- ret + theme(axis.ticks = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.x = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.y = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.length = grid::unit(5, "pt"))
  }
  
  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  
  ret <- ret + theme(axis.text.x=element_text(size=axis_text_size, margin=margin(t=0)))
  ret <- ret + theme(axis.text.y=element_text(size=axis_text_size, margin=margin(r=0)))
  ret <- ret + theme(axis.title=element_text(size=axis_title_size, family=axis_title_family))
  ret <- ret + theme(axis.title.x=element_text(hjust=xj, size=axis_title_size,
                                               family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(axis.title.y=element_text(hjust=yj, size=axis_title_size,
                                               family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(axis.title.y.right=element_text(hjust=yj, size=axis_title_size, angle=90,
                                                     family=axis_title_family, face=axis_title_face))
  ret <- ret + theme(strip.text=element_text(hjust=0, size=strip_text_size,
                                             face=strip_text_face, family=strip_text_family))
  ret <- ret + theme(panel.spacing=grid::unit(2, "lines"))
  ret <- ret + theme(plot.title=element_text(hjust=0, size=plot_title_size,
                                             margin=margin(b=plot_title_margin),
                                             family=plot_title_family, face=plot_title_face))
  ret <- ret + theme(plot.subtitle=element_text(hjust=0, size=subtitle_size,
                                                margin=margin(b=subtitle_margin),
                                                family=subtitle_family, face=subtitle_face))
  ret <- ret + theme(plot.caption=element_text(hjust=1, size=caption_size,
                                               margin=margin(t=caption_margin),
                                               family=caption_family, face=caption_face))
  ret <- ret + theme(plot.margin=plot_margin)
  
  ret
  
}
