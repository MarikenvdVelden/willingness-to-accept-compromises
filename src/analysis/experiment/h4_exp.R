# H4
issues <- unique(d$issue)
#principledness 2
for(i in 1:length(issues)){
  df <- d %>% 
    mutate(compromise = if_else(compromise=="yes", 0, 1),
           outcome = if_else(outcome == "negotiation", 0, 1),
           b = HT1_rescale,
           partner = recode(partner, 
                            "CDU" = 1,
                            "die Grünen"= 0,
                            "FDP" = 0,
                            "SPD" = 1)) %>%
    filter(issue == issues[i])
  if(i==1){
    h4 <- regression_ht1(df, compromise, outcome, b) %>%
      mutate(issue = issues[i])
  }  
  else{
    tmp <- regression_ht1(df, compromise, outcome, b) %>%
      mutate(issue = issues[i])
    h4 <- h4 %>% add_case(tmp) %>%
      mutate(type = "Principledness (2)")
  }
}

h4 <- h4 %>%
  filter(y != "DV3") %>% 
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
         b = HT1_rescale,
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1)) 
h4p <- pooled_regression_ht1(df, compromise, outcome, b, issue) %>% 
  filter(y != "DV3") %>% 
  mutate(issue = "Pooled Analysis",
         y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")),
         type = "Principledness (2)")

p3a <- h4 %>% 
  add_case(h4p) %>% 
  mutate(issue = factor(issue,
                        levels = c("SpeedLimit", "TopTax", "Pooled Analysis")),
         hyp = "Hypothesis 4 - Principledness (2)") %>% 
  ggplot(aes(x = b, 
             y = AME,
             color = issue,
             fill = issue,
             ymin = lower,
             ymax = upper,
             label = issue)) +
  geom_line() + 
  geom_ribbon(alpha = .2) +
  theme_ipsum() +
  labs(x = "Levels of Principledness \n (0 = Low, 4 = High)", y = "Average Marginal Effects of Being Steadfast",
  caption = "Visualized results are based upon OLS regression of variables of interested controlled for the unbalanced co-variates: 
       Degree of urbanization, employment, region of residence and birth, \n left-right position of the respondent, attitude towards the speed limit and top tax policies, \n and issue importance of top tax.
       Pooled Analyses have additonally the issues as a covariate added.") +
  facet_grid(hyp~y) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed")


#principledness1
for(i in 1:length(issues)){
  df <- d %>% 
    mutate(compromise = if_else(compromise=="yes", 0, 1),
           outcome = if_else(outcome == "negotiation", 0, 1),
           b = HT2,
           partner = recode(partner, 
                            "CDU" = 1,
                            "die Grünen"= 0,
                            "FDP" = 0,
                            "SPD" = 1)) %>%
    filter(issue == issues[i])
  if(i==1){
    h4b <- regression_ht2(df, compromise, outcome, b) %>%
      mutate(issue = issues[i])
  }  
  else{
    tmp <- regression_ht2(df, compromise, outcome, b) %>%
      mutate(issue = issues[i])
    h4b <- h4b %>% add_case(tmp) %>%
      mutate(type = "Principledness (1)")
  }
}

h4b <- h4b %>% 
  filter(y != "DV3") %>% 
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
         b = HT2,
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1)) 
h4bp <- pooled_regression_ht2(df, compromise, outcome, b, issue) %>% 
  filter(y != "DV3") %>% 
  mutate(issue = "Pooled Analysis",
         y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")),
         type = "Principledness (1)")


p3b <- h4b %>%
  add_case(h4bp) %>% 
  mutate(issue = factor(issue,
                        levels = c("SpeedLimit", "TopTax", "Pooled Analysis")),
         hyp = "Hypothesis 4 - Principledness (1)") %>% 
  ggplot(aes(x = b, 
             y = AME,
             color = issue,
             fill = issue,
             ymin = lower,
             ymax = upper,
             label = issue)) +
  geom_line() + 
  geom_ribbon(alpha = .2) +
  theme_ipsum() +
  labs(x = "Levels of Principledness \n (0 = Low, 4 = High)", y = "Average Marginal Effects of Being Steadfast",
       caption = "Visualized results are based upoon OLS regression of variables of interested controlled for the unbalanced co-variates: 
       Degree of urbanization, employment, region of residence and birth, \n left-right position of the respondent, attitude towards the speed limit and top tax policies, \n and issue importance of top tax.
       Pooled Analyses have additonally the issues as a covariate added.") +
  facet_grid(hyp~y) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, linewidth = .2, linetype = "dashed")

p3_2a <- h4p %>%
  mutate(issue = factor(issue,
                        levels = c("SpeedLimit", "TopTax", "Pooled Analysis")),
         hyp = "Hypothesis 4 - Principledness (2)") %>% 
  ggplot(aes(x = b, 
             y = AME,
             color = issue,
             fill = issue,
             ymin = lower,
             ymax = upper,
             label = issue)) +
  geom_line() + 
  geom_ribbon(alpha = .2) +
  theme_ipsum() +
  labs(x = "Levels of Principledness \n (0 = Low, 4 = High)", 
       y = "Average Marginal Effects of Being Steadfast",
       caption = "Visualized results are based upoon OLS regression of variables of interested controlled for the unbalanced co-variates: 
       Degree of urbanization, employment, region of residence and birth, \n left-right position of the respondent, attitude towards the speed limit and top tax policies, \n and issue importance of top tax.
       Pooled Analyses have additonally the issues as a covariate added.") +
  facet_grid(hyp~y) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols[3]) +
  scale_fill_manual(values = fig_cols[3]) +
  geom_hline(yintercept = 0, linewidth = .2, linetype = "dashed")

p3_2b <- h4bp %>%
  mutate(issue = factor(issue,
                        levels = c("SpeedLimit", "TopTax", "Pooled Analysis")),
         hyp = "Hypothesis 4 - Principledness (1)") %>% 
  ggplot(aes(x = b, 
             y = AME,
             color = issue,
             fill = issue,
             ymin = lower,
             ymax = upper,
             label = issue)) +
  geom_line() + 
  geom_ribbon(alpha = .2) +
  theme_ipsum() +
  labs(x = "Levels of Principledness \n (0 = Low, 4 = High)", 
       y = "Average Marginal Effects of Being Steadfast") +
  facet_grid(hyp~y) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols[3]) +
  scale_fill_manual(values = fig_cols[3]) +
  geom_hline(yintercept = 0, linewidth = .2, linetype = "dashed")
