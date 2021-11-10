# H4
issues <- unique(d$issue)
#principledness 2
for(i in 1:length(issues)){
  df <- d %>% 
    mutate(compromise = if_else(compromise=="yes", 0, 1),
           outcome = if_else(outcome == "negotiation", 1, 0),
           b = HT1,
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


#principledness1
for(i in 1:length(issues)){
  df <- d %>% 
    mutate(compromise = if_else(compromise=="yes", 0, 1),
           outcome = if_else(outcome == "negotiation", 1, 0),
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

p3a <- h4 %>%
  mutate(y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation"))) %>% 
  ggplot(aes(x = b, 
             y = AME,
             color = issue,
             fill = issue,
             ymin = lower,
             ymax = upper,
             label = issue)) +
  geom_line() + 
  geom_ribbon(alpha = .2) +
  theme_minimal() +
  labs(x = "Levels of Principledness \n (2 = Low, 14 = High)", y = "Average Marginal Effects of Levels of Principledness") +
  facet_grid(type~y) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed")

p3b <- h4b %>%
  mutate(y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation"))) %>% 
  ggplot(aes(x = b, 
             y = AME,
             color = issue,
             fill = issue,
             ymin = lower,
             ymax = upper,
             label = issue)) +
  geom_line() + 
  geom_ribbon(alpha = .2) +
  theme_minimal() +
  labs(x = "Levels of Principledness \n (0 = Low, 4 = High)", y = "Average Marginal Effects of Levels of Principledness") +
  facet_grid(type~y) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed")

df <- d %>% 
  mutate(compromise = if_else(compromise=="yes", 0, 1),
         outcome = if_else(outcome == "negotiation", 1, 0),
         b = HT1,
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1)) 
h4p <- pooled_regression_ht1(df, compromise, outcome, b, issue)

p3ap <- h4p %>%
  mutate(y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")),
         type = "Principledness (2)") %>% 
  ggplot(aes(x = b, 
             y = AME,
             ymin = lower,
             ymax = upper)) +
  geom_line(color = fig_cols[1]) + 
  geom_ribbon(alpha = .2, fill = fig_cols[1]) +
  theme_minimal() +
  labs(x = "Levels of Principledness \n (2 = Low, 14 = High) \n Pooled over Issues", y = "Average Marginal Effects of Levels of Principledness") +
  facet_grid(type~y) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed")

df <- d %>% 
  mutate(compromise = if_else(compromise=="yes", 0, 1),
         outcome = if_else(outcome == "negotiation", 1, 0),
         b = HT2,
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1)) 
h4bp <- pooled_regression_ht2(df, compromise, outcome, b, issue)

p3bp <- h4bp %>%
  mutate(y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")),
         type = "Principledness (1)") %>% 
  ggplot(aes(x = b, 
             y = AME,
             ymin = lower,
             ymax = upper)) +
  geom_line(color = fig_cols[1]) + 
  geom_ribbon(alpha = .2, fill = fig_cols[1]) +
  theme_minimal() +
  labs(x = "Levels of Principledness \n (0 = Low, 4 = High) \n Pooled over Issues", y = "Average Marginal Effects of Levels of Principledness") +
  facet_grid(type~y) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed")
