# Exploration with Political moderators
issues <- unique(d$issue)

#1. Pop att (var: POST)
for(i in 1:length(issues)){
  df <- d %>% 
    mutate(compromise = if_else(compromise=="yes", 0, 1),
           outcome = if_else(outcome == "negotiation", 0, 1),
           b = POST,
           partner = recode(partner, 
                            "CDU" = 1,
                            "die Grünen"= 0,
                            "FDP" = 0,
                            "SPD" = 1)) %>%
    filter(issue == issues[i])
  if(i==1){
    h_mod1 <- regression_ht6(df, compromise, outcome, b) %>%
      mutate(issue = issues[i])
  }  
  else{
    tmp <- regression_ht6(df, compromise, outcome, b) %>%
      mutate(issue = issues[i])
    h_mod1 <- h_mod1 %>% add_case(tmp) %>%
      mutate(type = "Populist Attitudes \n (1 = Low, 5 = High)")
  }
}

h_mod1 <- h_mod1 %>%
  mutate(y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation"))) 

df <- d %>% 
  mutate(compromise = if_else(compromise=="yes", 0, 1),
         outcome = if_else(outcome == "negotiation", 0, 1),
         b = POST,
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1)) 
h_mod1p <- pooled_regression_ht6(df, compromise, outcome, b, issue) %>% 
  mutate(issue = "Pooled Analysis",
         y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")),
         type = "Populist Attitudes \n (1 = Low, 5 = High)")

# 2. L/R (PT8)
for(i in 1:length(issues)){
  df <- d %>% 
    mutate(compromise = if_else(compromise=="yes", 0, 1),
           outcome = if_else(outcome == "negotiation", 0, 1),
           b = PT8,
           partner = recode(partner, 
                            "CDU" = 1,
                            "die Grünen"= 0,
                            "FDP" = 0,
                            "SPD" = 1)) %>%
    filter(issue == issues[i])
  if(i==1){
    h_mod2 <- regression_ht7(df, compromise, outcome, b) %>%
      mutate(issue = issues[i])
  }  
  else{
    tmp <- regression_ht7(df, compromise, outcome, b) %>%
      mutate(issue = issues[i])
    h_mod2 <- h_mod2 %>% add_case(tmp) %>%
      mutate(type = "Ideological Position \n (0 = Left, 10 = Right)")
  }
}

h_mod2 <- h_mod2 %>%
  mutate(y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation"))) 

df <- d %>% 
  mutate(compromise = if_else(compromise=="yes", 0, 1),
         outcome = if_else(outcome == "negotiation", 0, 1),
         b = PT8,
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1)) 
h_mod2p <- pooled_regression_ht7(df, compromise, outcome, b, issue) %>% 
  mutate(issue = "Pooled Analysis",
         y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")),
         type = "Ideological Position \n (0 = Left, 10 = Right)")

#3. Issue position: congruent with party or not? / congruent with outcome or not?
## PT1_1 = speed limit
## `PT1_2` = "Position Top Tax",
for(i in 1:length(issues)){
  df <- d %>% 
    mutate(compromise = if_else(compromise=="yes", 0, 1),
           outcome = if_else(outcome == "negotiation", 0, 1),
           b = 0,
           b = ifelse(issue=="SpeedLimit" & PT1_1==1, 1, b), #1==congruent
           b = ifelse(issue=="TopTax" & PT1_2==1, 1, b),
           partner = recode(partner, 
                            "CDU" = 1,
                            "die Grünen"= 0,
                            "FDP" = 0,
                            "SPD" = 1)) %>%
    filter(issue == issues[i])
  if(i==1){
    h_mod3 <- regression_ht4(df, compromise, outcome, b) %>%
      mutate(issue = issues[i])
  }  
  else{
    tmp <- regression_ht4(df, compromise, outcome, b) %>%
      mutate(issue = issues[i])
    h_mod3 <- h_mod3 %>% add_case(tmp) %>%
      mutate(type = "Issue Congruence")
  }
}

h_mod3 <- h_mod3 %>%
  mutate(y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")),
         b = recode(b,
                    `0` = "No Congruent Position",
                    `1` = "Congruent Position")) 

df <- d %>% 
  mutate(compromise = if_else(compromise=="yes", 0, 1),
         outcome = if_else(outcome == "negotiation", 0, 1),
         b = 0,
         b = ifelse(issue=="SpeedLimit" & PT1_1==1, 1, b), #1==congruent
         b = ifelse(issue=="TopTax" & PT1_2==1, 1, b),
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1)) 
h_mod3p <- pooled_regression_ht4(df, compromise, outcome, b, issue) %>% 
  mutate(issue = "Pooled Analysis",
         y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")),
         type = "Issue Congruence",
         b = recode(b,
                    `0` = "No Congruent Position",
                    `1` = "Congruent Position"))


#4. Issue importance
## PT3_1` = "Salience Speed Limit",
## `PT3_2` = "Salience Top Tax",
for(i in 1:length(issues)){
  df <- d %>% 
    mutate(compromise = if_else(compromise=="yes", 0, 1),
           outcome = if_else(outcome == "negotiation", 0, 1),
           b = 0,
           b = ifelse(issue=="SpeedLimit", PT3_1, b), #1==congruent
           b = ifelse(issue=="TopTax", PT3_2, b),
           partner = recode(partner, 
                            "CDU" = 1,
                            "die Grünen"= 0,
                            "FDP" = 0,
                            "SPD" = 1)) %>%
    filter(issue == issues[i])
  if(i==1){
    h_mod4 <- regression_ht5(df, compromise, outcome, b) %>%
      mutate(issue = issues[i])
  }  
  else{
    tmp <- regression_ht5(df, compromise, outcome, b) %>%
      mutate(issue = issues[i])
    h_mod4 <- h_mod4 %>% add_case(tmp) %>%
      mutate(type = "Issue Salience \n (0 = Low, 11 = High)")
  }
}

h_mod4 <- h_mod4 %>%
  mutate(y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation"))) 

df <- d %>% 
  mutate(compromise = if_else(compromise=="yes", 0, 1),
         outcome = if_else(outcome == "negotiation", 0, 1),
         b = 0,
         b = ifelse(issue=="SpeedLimit", PT3_1, b), #1==congruent
         b = ifelse(issue=="TopTax", PT3_2, b),
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1)) 
h_mod4p <- pooled_regression_ht5(df, compromise, outcome, b, issue) %>% 
  mutate(issue = "Pooled Analysis",
         y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")),
         type = "Issue Salience \n (0 = Low, 11 = High)")

#5. Partij keuze (D6)
for(i in 1:length(issues)){
  df <- d %>% 
    drop_na(D6) %>% 
    mutate(compromise = if_else(compromise=="yes", 0, 1),
           outcome = if_else(outcome == "negotiation", 0, 1),
           b = D6,
           b = factor(b,
                      levels = c("Didn't vote/Not eligible",
                                 "AfD", "CDU/CSU", "FDP", "Greens",
                                 "Left", "SPD", "Other party")),
           partner = recode(partner, 
                            "CDU" = 1,
                            "die Grünen"= 0,
                            "FDP" = 0,
                            "SPD" = 1)) %>%
    filter(issue == issues[i])
  if(i==1){
    h_mod5 <- regression_ht8(df, compromise, outcome, b) %>%
      mutate(issue = issues[i])
  }  
  else{
    tmp <- regression_ht8(df, compromise, outcome, b) %>%
      mutate(issue = issues[i])
    h_mod5 <- h_mod5 %>% add_case(tmp) %>%
      mutate(type = "Party Choice")
  }
}

h_mod5 <- h_mod5 %>%
  mutate(y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation"))) 

df <- d %>% 
  mutate(compromise = if_else(compromise=="yes", 0, 1),
         outcome = if_else(outcome == "negotiation", 0, 1),
         b = D6,
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1)) 
h_mod5p <- pooled_regression_ht8(df, compromise, outcome, b, issue) %>% 
  mutate(issue = "Pooled Analysis",
         y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")),
         type = "Party Choice")

#6. Politieke Interesse (PT7)
for(i in 1:length(issues)){
  df <- d %>% 
    mutate(compromise = if_else(compromise=="yes", 0, 1),
           outcome = if_else(outcome == "negotiation", 0, 1),
           b = PT7,
           partner = recode(partner, 
                            "CDU" = 1,
                            "die Grünen"= 0,
                            "FDP" = 0,
                            "SPD" = 1)) %>%
    filter(issue == issues[i])
  if(i==1){
    h_mod6 <- regression_ht6(df, compromise, outcome, b) %>%
      mutate(issue = issues[i])
  }  
  else{
    tmp <- regression_ht6(df, compromise, outcome, b) %>%
      mutate(issue = issues[i])
    h_mod6 <- h_mod4 %>% add_case(tmp) %>%
      mutate(type = "Political Interest \n (1 = Low, 5 = High)")
  }
}

h_mod6 <- h_mod6 %>%
  mutate(y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation"))) 

df <- d %>% 
  mutate(compromise = if_else(compromise=="yes", 0, 1),
         outcome = if_else(outcome == "negotiation", 0, 1),
         b = POST,
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1)) 
h_mod6p <- pooled_regression_ht5(df, compromise, outcome, b, issue) %>% 
  mutate(issue = "Pooled Analysis",
         y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")),
         type = "Political Interest \n (1 = Low, 5 = High)")

## plot
mod_a <- h_mod1 %>% 
  add_case(h_mod1p) %>% 
  add_case(h_mod2) %>% 
  add_case(h_mod2p) %>% 
  add_case(h_mod4) %>% 
  add_case(h_mod4p) %>% 
  add_case(h_mod6) %>% 
  add_case(h_mod6p) %>%     
  mutate(issue = factor(issue,
                        levels = c("SpeedLimit", "TopTax", "Pooled Analysis"))) %>% 
  ggplot(aes(x = b, 
             y = AME,
             color = issue,
             fill = issue,
             ymin = lower,
             ymax = upper,
             group = issue,
             label = issue)) +
  geom_line() + 
  geom_ribbon(alpha = .2) +
  theme_ipsum() +
  labs(x = "", y = "Average Marginal Effects of Being Steadfast") +
  facet_grid(y~type, scales = "free") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed")

mod_b <- h_mod3 %>% 
  add_case(h_mod3p) %>% 
  add_case(h_mod5) %>% 
  add_case(h_mod5p) %>% 
  ggplot(aes(x = b, 
             y = AME,
             color = issue,
             fill = issue,
             ymin = lower,
             ymax = upper,
             group = issue,
             label = issue)) +
  geom_point(position = position_dodge(0.5)) + 
  geom_errorbar(position = position_dodge(0.5), width = 0) +
  theme_ipsum() +
  labs(x = "", y = "Average Marginal Effects of Being Steadfast") +
  facet_grid(y~type, scales = "free") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed")
