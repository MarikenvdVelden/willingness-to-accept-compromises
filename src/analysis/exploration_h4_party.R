# H4
issues <- unique(d$issue)
party <- unique(d$S1)

#principledness 2
for(i in 1:length(issues)){
  for(j in 1:length(party)){
    df <- d %>% 
      mutate(compromise = if_else(compromise=="yes", 0, 1),
             outcome = if_else(outcome == "negotiation", 0, 1),
             b = HT1,
             partner = recode(partner, 
                              "CDU" = 1,
                              "die Grünen"= 0,
                              "FDP" = 0,
                              "SPD" = 1)) %>%
      filter(issue == issues[i] & S1 == party[j])
  if(i==1 & j==1){
    h4_e <- regression_ht1_party(df, compromise, outcome, b) %>%
      mutate(issue = issues[i],
             party = party[j])
  }  
  else{
    tmp <- regression_ht1_party(df, compromise, outcome, b) %>%
      mutate(issue = issues[i],
             party = party[j])
    h4_e <- h4_e %>% add_case(tmp) %>%
      mutate(type = "Principledness (2)")
    }
  }
}

h4_e <- h4_e %>%
  mutate(y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")))

#pooled
d2 <- d %>% 
  mutate(compromise = if_else(compromise=="yes", 0, 1),
         outcome = if_else(outcome == "negotiation", 0, 1),
         b = HT1,
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1)) 

for(i in 1:length(party)){
  df <- d2 %>% filter(S1 == party[i])
  if(i==1){
    h4p_e <- pooled_regression_ht1_party(df, compromise, outcome, b, issue) %>% 
      mutate(issue = "Pooled Analysis",
             y = recode(y,
                        `DV1` = "DV: Trust",
                        `DV2` = "DV: Credibility",
                        `DV3` = "DV: Representation"),
             y = factor(y, 
                        levels = c("DV: Trust", "DV: Credibility",
                                   "DV: Representation")),
             type = "Principledness (2)",
             party = party[i])
  }
  else{
    tmp <- pooled_regression_ht1_party(df, a, b, issue) %>%
      mutate(issue = "Pooled Analysis",
             y = recode(y,
                        `DV1` = "DV: Trust",
                        `DV2` = "DV: Credibility",
                        `DV3` = "DV: Representation"),
             y = factor(y, 
                        levels = c("DV: Trust", "DV: Credibility",
                                   "DV: Representation")),
             type = "Principledness (2)",
             party = party[i])
    h4p_e <- h4p_e %>% add_case(tmp)
  }
}

#Viz
p4a_e1 <- h4_e %>% 
  add_case(h4p_e) %>% 
  mutate(issue = factor(issue,
                        levels = c("SpeedLimit", "TopTax", "Pooled Analysis"))) %>% 
  filter(y == "DV: Trust") %>% 
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
  labs(x = "Levels of Principledness \n (2 = Low, 14 = High)", 
       y = "Average Marginal Effects of Being Steadfast",
       subtitle = "DV: Trust") +
  facet_grid(issue~party) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed")

p4a_e2 <- h4_e %>% 
  add_case(h4p_e) %>% 
  mutate(issue = factor(issue,
                        levels = c("SpeedLimit", "TopTax", "Pooled Analysis"))) %>% 
  filter(y == "DV: Credibility") %>% 
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
  labs(x = "Levels of Principledness \n (2 = Low, 14 = High)", 
       y = "Average Marginal Effects of Being Steadfast",
       subtitle = "DV: Credibility") +
  facet_grid(issue~party) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed")

p4a_e3 <- h4_e %>% 
  add_case(h4p_e) %>% 
  mutate(issue = factor(issue,
                        levels = c("SpeedLimit", "TopTax", "Pooled Analysis"))) %>% 
  filter(y == "DV: Representation") %>% 
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
  labs(x = "Levels of Principledness \n (2 = Low, 14 = High)", 
       y = "Average Marginal Effects of Being Steadfast",
       subtitle = "DV: Representation") +
  facet_grid(issue~party) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed")


#principledness1
for(i in 1:length(issues)){
  for(j in 1:length(party)){
    df <- d %>% 
      mutate(compromise = if_else(compromise=="yes", 0, 1),
             outcome = if_else(outcome == "negotiation", 0, 1),
             b = HT2,
             partner = recode(partner, 
                              "CDU" = 1,
                              "die Grünen"= 0,
                              "FDP" = 0,
                              "SPD" = 1)) %>%
      filter(issue == issues[i] & S1 == party[j])
    if(i==1 & j==1){
      h4b_e <- regression_ht2_party(df, compromise, outcome, b) %>%
        mutate(issue = issues[i],
               party = party[j])
    } 
    else{
      tmp <- regression_ht2_party(df, compromise, outcome, b) %>%
        mutate(issue = issues[i],
               party = party[j])
      h4b_e <- h4b_e %>% add_case(tmp) %>%
        mutate(type = "Principledness (1)")
    }
  }
}

h4b_e <- h4b_e %>% 
  mutate(y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation"))) 

#pooled
d2 <- d %>% 
  mutate(compromise = if_else(compromise=="yes", 0, 1),
         outcome = if_else(outcome == "negotiation", 0, 1),
         b = HT2,
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1)) 

for(i in 1:length(party)){
  df <- d2 %>% filter(S1 == party[i])
  if(i==1){
    h4p_be <- pooled_regression_ht2_party(df, compromise, outcome, b, issue) %>% 
      mutate(issue = "Pooled Analysis",
             y = recode(y,
                        `DV1` = "DV: Trust",
                        `DV2` = "DV: Credibility",
                        `DV3` = "DV: Representation"),
             y = factor(y, 
                        levels = c("DV: Trust", "DV: Credibility",
                                   "DV: Representation")),
             type = "Principledness (2)",
             party = party[i])
  }
  else{
    tmp <- pooled_regression_ht2_party(df, a, b, issue) %>%
      mutate(issue = "Pooled Analysis",
             y = recode(y,
                        `DV1` = "DV: Trust",
                        `DV2` = "DV: Credibility",
                        `DV3` = "DV: Representation"),
             y = factor(y, 
                        levels = c("DV: Trust", "DV: Credibility",
                                   "DV: Representation")),
             type = "Principledness (2)",
             party = party[i])
    h4p_be <- h4p_be %>% add_case(tmp)
  }
}

#Viz
p4b_e1 <- h4b_e %>% 
  add_case(h4p_be) %>% 
  mutate(issue = factor(issue,
                        levels = c("SpeedLimit", "TopTax", "Pooled Analysis"))) %>% 
  filter(y == "DV: Trust") %>% 
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
  labs(x = "Levels of Principledness \n (0 = Low, 1 = High)", 
       y = "Average Marginal Effects of Being Steadfast",
       subtitle = "DV: Trust") +
  facet_grid(issue~party) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed")

p4b_e2 <- h4b_e %>% 
  add_case(h4p_be) %>% 
  mutate(issue = factor(issue,
                        levels = c("SpeedLimit", "TopTax", "Pooled Analysis"))) %>% 
  filter(y == "DV: Credibility") %>% 
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
  labs(x = "Levels of Principledness \n (0 = Low, 1 = High)", 
       y = "Average Marginal Effects of Being Steadfast",
       subtitle = "DV: Credibility") +
  facet_grid(issue~party) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed")

p4b_e3 <- h4b_e %>% 
  add_case(h4p_be) %>% 
  mutate(issue = factor(issue,
                        levels = c("SpeedLimit", "TopTax", "Pooled Analysis"))) %>% 
  filter(y == "DV: Representation") %>% 
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
  labs(x = "Levels of Principledness \n (0 = Low, 1 = High)", 
       y = "Average Marginal Effects of Being Steadfast",
       subtitle = "DV: Representation") +
  facet_grid(issue~party) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed")
